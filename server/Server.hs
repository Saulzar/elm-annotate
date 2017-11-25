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

import System.FilePath
import System.Directory

import Data.Char (toLower)

import qualified Data.IntMap as M
import Data.IntMap (IntMap)

import Data.Aeson

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
  , document   :: Maybe T.Document
  } deriving (Generic)

data State    = State
  { clients :: IntMap Client
  , dataset :: Dataset
  } deriving (Generic)


type ClientM a = ReaderT (ClientId, MVar State) IO a
data Error = DecodeError | MissingClient
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
    Nothing -> liftIO (throw DecodeError)


readConfig :: FilePath -> IO Config
readConfig path = tryDecode =<< BS.readFile (path </> "config.json")


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



findImages :: Config -> FilePath -> IO [ImageInfo]
findImages config root = do
  contents <- listDirectory root
  let matching = filter (validExtension (config ^. field @"extensions")) contents
  catMaybes <$> mapM getInfo matching

readDataset :: FilePath -> IO Dataset
readDataset root = do
  config <- readConfig root
  files <- findImages config root
  return Dataset { path = root, images = files, config = config }


getState :: ClientM State
getState = do
  (k, stateRef) <- ask
  liftIO (readMVar stateRef)


getClient :: ClientM Client
getClient = do
  (k, stateRef) <- ask
  state <- liftIO (readMVar stateRef)

  case M.lookup k (state ^. field @"clients") of
    Just client -> return client
    Nothing -> liftIO (throw MissingClient)


getConnection :: ClientM WS.Connection
getConnection = view (field @"connection") <$> getClient

getDataset :: ClientM Dataset
getDataset = view (field @"dataset") <$> getState


respond :: Response -> ClientM ()
respond response = do
  conn <- getConnection
  liftIO $ WS.sendTextData conn (encode response)



request :: ClientM Request
request = do
  conn <- getConnection
  tryDecode =<< liftIO (WS.receiveData conn)

client ::   ClientM ()
client = forever $ do
  r <- request
  case r of
      ReqPing n -> respond (RespPong n)
      ReqDataset -> respond =<< RespDataset <$> getDataset
      _ -> return ()


wsApp :: MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  -- WS.forkPingThread conn 30
  Exception.finally
    (runReaderT client (clientId, stateRef))
    (disconnectClient clientId stateRef)


main :: IO ()
main = do

  dataset <- readDataset "trees"
  state <- newMVar $ State M.empty dataset
  Warp.run 3000 $ WS.websocketsOr
    WS.defaultConnectionOptions
    (wsApp state)
    httpApp

httpApp :: Wai.Application
httpApp = Static.staticApp (Static.defaultWebAppSettings "html")
