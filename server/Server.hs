module Main where


import Control.Monad
import Control.Monad.Reader

import Data.List
import Data.Maybe

import Control.Exception
import Data.Typeable

import Control.Lens

import GHC.Generics
import Data.Generics.Product
import Data.Generics.Labels()

import System.FilePath
import System.Directory

import System.Environment (getArgs)

import Options as Opt

import Data.Char (toLower)

import qualified Data.Map as M
import Data.Map (Map)

import Data.Aeson hiding (Object)

import qualified Data.ByteString.Lazy                      as BS
import Data.ByteString.Lazy (ByteString)

import Control.Concurrent
import qualified Control.Exception              as Exception

import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as Static


import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

import qualified Safe as Safe

import qualified Types as T
import Types

type ClientId = Int

data Client  = Client
  { connection :: WS.Connection
  , document   :: Maybe (String, T.Document)
  } deriving (Generic)

data State    = State
  { clients :: Map Int Client
  , dataset :: Dataset
  } deriving (Generic)


type ClientM a = ReaderT (ClientId, MVar State) IO a
data Error = ClientMissing ClientId | DocumentNotOpen | BadEdit String Edit  | DecodeError ByteString | RootDirectoryMissing String
   deriving (Show, Typeable)

instance Exception Error

nextClient :: State -> ClientId
nextClient state = fromMaybe 0 (fst . fst <$>  M.maxViewWithKey (clients state))


connectClient :: WS.Connection -> MVar State -> IO ClientId
connectClient conn stateRef = modifyMVar stateRef $ \state -> do
  let clientId = nextClient state
      client = Client conn Nothing
  print ("Client connected", clientId)

  return (state & field @"clients" %~ (M.insert clientId client), clientId)


disconnectClient :: ClientId -> MVar State -> IO ()
disconnectClient clientId stateRef = modifyMVar_ stateRef $ \state -> do
  print ("Client disconnected", clientId)
  return $ state & field @"clients" %~ M.delete clientId


tryDecode :: (MonadIO m, FromJSON a) => ByteString -> m a
tryDecode str = case decode str of
    Just req -> return req
    Nothing -> liftIO (throw $ DecodeError str)


readConfig :: FilePath -> IO Config
readConfig path = do
  r <- decode <$> BS.readFile (path </> "config.json")
  case r of
    Nothing -> error ("failed to decode config: " ++ path)
    Just config -> return config


validExtension :: [String] -> FilePath -> Bool
validExtension exts filename = any (\e -> map toLower e == ext) exts where
  ext = map toLower (takeExtension filename)



getInfo :: FilePath -> IO (Maybe ImageInfo)
getInfo path = do
  exists <- doesFileExist path
  annotated <- doesFileExist (path ++ ".json")
  return $ if exists
    then Just (ImageInfo {file = takeFileName path, annotated = annotated})
    else Nothing


staticRoot :: String
staticRoot = "html"


findImages :: Config -> FilePath -> IO [ImageInfo]
findImages config root = do
  contents <- listDirectory root
  let matching = filter (validExtension (config ^. field @"extensions")) contents
  catMaybes <$> mapM (getInfo . (root </>)) matching

readDataset :: FilePath -> IO Dataset
readDataset root = do
  config <- readConfig (staticRoot </> root)
  files <- findImages config (staticRoot </> root)
  return Dataset { path = root, images = files, config = config }


getState :: ClientM State
getState = do
  (k, stateRef) <- ask
  liftIO (readMVar stateRef)


lookupClient :: State -> ClientId -> Maybe Client
lookupClient state clientId = M.lookup clientId (state ^. field @"clients")





getClient :: ClientM Client
getClient = do
  (clientId, stateRef) <- ask
  state <- liftIO (readMVar stateRef)
  case lookupClient state clientId of
    Nothing -> liftIO (throw $ ClientMissing clientId)
    Just client -> return client



filePath :: State -> FilePath -> FilePath
filePath state file = staticRoot </> (state ^. field @"dataset" . field @"path") </> file


defaultDocument ::  Document
defaultDocument = Document
  { undos = []
  , redos = []
  , instances = M.empty
  }


loadDocument :: State -> FilePath -> IO (Maybe Document)
loadDocument state file = do
  exists <- doesFileExist path
  if exists
    then  decode <$> BS.readFile path
    else return Nothing

  where path = filePath state file

openDocument :: FilePath -> ClientM Document
openDocument file = do
  (clientId, stateRef) <- ask
  doc <- liftIO $ modifyMVar stateRef $ \state -> do
    doc <- fromMaybe defaultDocument <$> loadDocument state file
    return (state & _document clientId .~ Just (file, doc), doc )

  return doc


flushDocument :: ClientM ()
flushDocument = do
  state <- getState
  mDoc <- view (field @"document") <$> getClient

  forM_ mDoc $ \(file, doc) -> do
    liftIO $ print (filePath state file)
    liftIO $ BS.writeFile (filePath state file)  (encode doc)


_document :: ClientId -> Traversal' State (Maybe (FilePath, Document))
_document clientId = field @"clients" . at clientId . traverse . field @"document"


modifyDocument :: (Document -> Either String Document) -> ClientM (Maybe String)
modifyDocument f = do
  (clientId, stateRef) <- ask
  liftIO $ modifyMVar stateRef $ \state -> return $
    case (firstOf (_document clientId . traverse) state) of
      Nothing     -> (state, Just "document not open")
      (Just doc)  -> case f (snd doc) of
          Left err  -> (state, Just err)
          Right doc' -> (state & (_document clientId . traverse . _2) .~ doc', Nothing)



runEdit :: Edit -> Document -> Either String Document
runEdit edit doc = do
  (inverse, doc') <- applyEdit edit doc

  return $ doc & field @"undos" %~ (inverse:)



accumEdits :: Edit -> ([Edit], Document) -> Either String ([Edit], Document)
accumEdits edit (inverses, doc) = do
  (inv, doc') <- applyEdit edit doc
  return (inv : inverses, doc')


transformObj :: Float -> Vec2 -> Object -> Object
transformObj = undefined

instance Num Vec2 where
  negate (Vec2 x y) = Vec2 (-x) (-y)

applyEdit :: Edit -> Document -> Either String (Edit, Document)
applyEdit edit doc =  case edit of
  Add k object -> return (Delete k, doc & #instances %~ M.insert k object)
  Delete k     -> case (M.lookup k (doc ^. #instances)) of
    Nothing     -> Left ("delete - key not found: " ++ show k)
    Just object -> return (Add k object, doc & #instances %~ M.delete k)
  Transform ks s v -> return
    ( Transform ks (1/s) (negate v)
    , foldr (\k -> over (#instances . at k . traverse) (transformObj s v)) doc ks)

  Many edits -> over _1 Many <$> foldM (flip accumEdits) ([], doc) edits


getConnection :: ClientM WS.Connection
getConnection = view (field @"connection") <$> getClient

getDataset :: ClientM Dataset
getDataset = view (field @"dataset") <$> getState


respond :: Response -> ClientM ()
respond response = do
  liftIO $ print response

  conn <- getConnection
  liftIO $ WS.sendTextData conn (encode response)



request :: ClientM Request
request = do
  conn <- getConnection
  tryDecode =<< liftIO (WS.receiveData conn)

client ::   ClientM ()
client = forever $ do
  req <- request
  liftIO $ print req
  case req of
      ReqPing n -> respond (RespPong n)
      ReqDataset -> respond =<< RespDataset <$> getDataset
      ReqOpen file -> do
        doc <- openDocument (file ++ ".json")
        respond (RespOpen file doc)

      ReqEdit e -> do
        maybeErr <- modifyDocument (runEdit e)
        forM_ maybeErr $ \err -> liftIO (throw (BadEdit err e))

        flushDocument
      -- _ -> return ()


wsApp :: MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (runReaderT client (clientId, stateRef))
    (disconnectClient clientId stateRef)


defaultConfig :: Config
defaultConfig = Config
  { extensions = [".png", ".jpg", ".jpeg"]
  }

initConfig :: String -> IO ()
initConfig root = do
    exists <- doesFileExist configFile
    when (not exists) $ do
      BS.writeFile configFile (encode defaultConfig)

    where
      configFile = staticRoot </> root </> "config.json"




main :: IO ()
main = do
  opts <- Opt.getArgs

  let root = Opt.root opts

  exists <- doesDirectoryExist (staticRoot </> root)
  when (not exists) $ throw (RootDirectoryMissing root)
  when (Opt.create opts) $ initConfig root

  dataset <- readDataset root

  state <- newMVar $ State M.empty dataset
  Warp.run 3000 $ WS.websocketsOr
    WS.defaultConnectionOptions
    (wsApp state)
    httpApp

httpApp :: Wai.Application
httpApp = Static.staticApp (Static.defaultWebAppSettings staticRoot)
