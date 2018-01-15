module Main where


import Control.Monad
import Control.Monad.IO.Class

import Control.Concurrent.STM
import GHC.Conc

import Control.Concurrent.Log

import Data.List
import Data.Maybe
import Data.Foldable

import Control.Exception
import Data.Typeable
import Data.Time.Clock

import Control.Lens

import GHC.Generics

import System.FilePath
import System.Directory
import System.IO
import System.Environment (getArgs)

import Options as Opt

import Data.Char (toLower, isSpace)
import Data.Void (Void)

import qualified Data.Map as M
import Data.Map (Map)

import qualified Data.Set as S
import Data.Set (Set)

import Data.Aeson hiding (Object)

import qualified Data.ByteString.Lazy                      as BS
import Data.ByteString.Lazy (ByteString)

import Control.Concurrent
import qualified Control.Exception              as Exception
-- import Codec.Picture as Codec
-- import Codec.Picture.Metadata as Codec

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad.Combinators

import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as Static


import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

import qualified Types as T

import Types
import AppState

import Servant
import Servant.Utils.StaticFiles

data ClientAction = ClientClose | ClientSend ServerMsg deriving (Show, Generic)

data Client  = Client
  { connection :: TChan ClientAction
  , document   :: Maybe String
  } deriving (Generic)


type Clients = TVar (Map Int Client)
type Documents = TVar (Map DocName [ClientId])

type LogMsg = String


data Env    = Env
  { clients     :: Clients
  , documents   :: Documents
  , state       :: Log AppState
  , root        :: FilePath
  , logChan     :: TChan LogMsg
  } deriving (Generic)


data Error = LogError String | RootDirectoryMissing FilePath | DecodeError ByteString
   deriving (Show, Typeable)

instance Exception Error



nextClient :: Map Int Client -> ClientId
nextClient m = fromMaybe 0 (succ . fst . fst <$>  M.maxViewWithKey m)


sendHello :: Env -> ClientId -> STM ()
sendHello env clientId = do
  ds <- getDataset <$> readCurrent (env ^. #state)
  sendClient env clientId (ServerHello clientId ds)



connectClient :: Env -> WS.Connection ->  IO ClientId
connectClient env conn = do
  (clientId, chan) <- atomically $ do
    clientId <- nextClient <$> readTVar (env ^. #clients)
    chan <- newTChan
    modifyTVar (env ^. #clients) (M.insert clientId (Client chan Nothing))
    writeLog env $ "client connected: " ++ show clientId
    return (clientId, chan)

  clientId <$ forkIO (sendThread chan)

  where
    sendThread chan = do
      action <- atomically $ readTChan chan
      case action of
        ClientClose   -> return ()
        ClientSend msg -> liftIO $ WS.sendTextData conn (encode msg) >> sendThread chan


withClient :: Clients -> ClientId -> (Client -> STM a) -> STM (Maybe a)
withClient clients clientId  f = do
  mClient <- M.lookup clientId <$> readTVar clients
  traverse f mClient


disconnectClient :: Env -> ClientId -> IO ()
disconnectClient env clientId = atomically $ do
  withClient (env ^. #clients) clientId $ \Client {..} -> do
    writeTChan connection ClientClose

    closeDocument env clientId
    time <- unsafeIOToSTM getCurrentTime
    broadcast env (ServerOpen Nothing clientId time)

  modifyTVar (view #clients env) (M.delete clientId)
  writeLog env $ "disconnected: " ++ show clientId



tryDecode :: (MonadIO m, FromJSON a) => ByteString -> m a
tryDecode str = case decode str of
    Just req -> return req
    Nothing -> liftIO (throw $ DecodeError str)



-- flushDocument :: MonadIO m => AcidAppState Storage -> FilePath -> FilePath -> m ()
-- flushDocument storage root filename = do
--   (info, mDoc) <- query storage (GetDocument filename)
--   forM_ mDoc $ \doc -> do
--     liftIO $ BS.writeFile (root </> filename) (encode (info, doc))
--

at' :: (At m, Applicative f) =>
     Index m -> (IxValue m -> f (IxValue m)) -> m -> f m
at' i = at i . traverse



respond :: MonadIO m => WS.Connection -> T.ServerMsg -> m ()
respond conn msg = liftIO $ WS.sendTextData conn (encode msg)



clientDoc :: ClientId -> Traversal' (Map ClientId Client) DocName
clientDoc clientId = at' clientId . #document . traverse


closeDocument :: Env -> ClientId -> STM ()
closeDocument (Env {..}) clientId  = (^? clientDoc clientId) <$> readTVar clients >>= traverse_ withDoc
  where
    withDoc docName = do
        refs <- M.lookup docName <$> readTVar documents
        modifyTVar documents (M.update removeClient docName)

    removeClient cs = case (filter (/= clientId) cs) of
        []  -> Nothing
        cs' -> Just cs'

        --   (#documents . at' doc ^?) <$> readCurrent state >>= traverse_ (\doc -> do
        --     writeTChan docWriter (DocFlush docName doc))





openDocument :: Env -> ClientId -> DocName -> STM ()
openDocument env@(Env {..}) clientId docName = do
  closeDocument env clientId

  modifyTVar clients (at' clientId . #document .~ Just docName)
  modifyTVar documents ( M.alter addClient docName)

  time <- unsafeIOToSTM getCurrentTime

  updateLog state (CmdModified docName time)
  broadcast env (ServerOpen (Just docName) clientId time)

    where
      addClient = \case
        Just cs -> Just (clientId:cs)
        Nothing -> Just [clientId]


makeEdit :: Env -> DocName -> Edit -> STM ()
makeEdit env@(Env {..}) docName edit = do
  updateLog state (CmdEdit docName edit)

  clients <- getEditing <$> readTVar documents
  for_ clients $ \clientId ->
    sendClient env clientId (ServerEdit docName edit)

  where
    getEditing = fromMaybe [] . M.lookup docName


recieveLoop :: Env -> WS.Connection -> ClientId -> IO ()
recieveLoop env conn clientId = do
  atomically $ sendHello env clientId
  forever $ do
    req <- tryDecode =<< liftIO (WS.receiveData conn)
    liftIO $ print req
    case req of
        ClientOpen file -> atomically $ openDocument env clientId file



sendClient :: Env -> ClientId -> T.ServerMsg -> STM ()
sendClient env clientId msg = void $ do
  withClient (env ^. #clients) clientId $ \Client {..} ->
    writeTChan connection (ClientSend msg)



broadcast :: Env -> T.ServerMsg -> STM ()
broadcast env msg = do
  clients <- readTVar (env ^. #clients)
  for_ clients $ \Client {..} ->
    writeTChan connection (ClientSend msg)

websocketServer :: Env -> WS.ServerApp
websocketServer env pending = do
  conn <- WS.acceptRequest pending

  clientId <- connectClient env conn

  WS.forkPingThread conn 30
  Exception.finally
    (recieveLoop env conn clientId)
    (disconnectClient env clientId)



type Api =
  "ws" :> Raw
  :<|> "images" :> Raw
  :<|> Raw


server :: Env -> Server Api
server env =
  withDefault (websocketServer env)
  :<|> serveDirectoryWebApp (env ^. #root)
  :<|> serveDirectoryWebApp "html"


withDefault :: WS.ServerApp -> Server Raw
withDefault ws = Tagged $ WS.websocketsOr WS.defaultConnectionOptions ws backupApp
  where backupApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a WebSocket request"






validExtension :: [String] -> FilePath -> Bool
validExtension exts filename = any (\e -> map toLower e == ext) exts where
  ext = map toLower (takeExtension filename)


findImages :: Config -> FilePath -> IO [DocName]
findImages config root = do
  contents <- listDirectory root
  return $ filter (validExtension (config ^. #extensions)) contents


-- imageInfo :: FilePath -> IO (Maybe DocInfo)
-- imageInfo filename = do
--   info <- docInfo <$> Codec.readImageWithMetadata filename
--   info <$ print (filename, info)
--   where
--     docInfo (Left _)              = Nothing
--     docInfo (Right (_, metadata)) = toInfo <$>
--       (Codec.lookup Width metadata) <*> (Codec.lookup Height metadata)
--
--     toInfo w h = DocInfo Nothing False (fromIntegral w, fromIntegral h)

-- /home/oliver/trees/_DSC2028.JPG JPEG 1600x1064 1600x1064+0+0 8-bit sRGB 958KB 0.000u 0:00.000

defaultInfo :: Dim -> DocInfo
defaultInfo dim = DocInfo
  { modified = Nothing
  , included = False
  , imageSize = dim
  }

type Parser = Parsec Void String

parseIdentify :: Parser (FilePath, String, Dim)
parseIdentify = do
  filename <- parseFilename
  space
  code <- fileCode
  space
  dim <- parseDim
  many anyChar
  return (filename, code, dim)


parseFilename :: Parser String
parseFilename = takeWhile1P Nothing (not . isSpace) <?> "word"

fileCode :: Parser String
fileCode = some letterChar <?> "file code"

parseDim :: Parser Dim
parseDim = do
  w <- decimal
  char 'x'
  h <- decimal
  return (w, h)


imageInfo :: FilePath -> IO (Maybe DocInfo)
imageInfo filename = do
  (exit, out, _) <- readProcessWithExitCode "identify" [filename] ""
  return $ case exit of
    ExitSuccess -> toInfo <$> parseMaybe parseIdentify out
    _           -> Nothing

    where
      toInfo (_, _, dim) = defaultInfo dim

findNewImages :: Config -> FilePath -> Map DocName DocInfo -> IO [(DocName, DocInfo)]
findNewImages config root existing = do
  images <- findImages config root

  catMaybes <$> (forM (filter (flip M.notMember existing) images) $ \image -> do
    fmap (image, ) <$> imageInfo (root </> image))



writeLog :: Env -> LogMsg -> STM ()
writeLog env msg = writeTChan (env ^. #logChan) msg

startLogger :: Handle -> IO (TChan LogMsg)
startLogger handle = do
  logChan   <- atomically newTChan

  forkIO $ forever $ do
    msg <- atomically $ readTChan logChan
    hPutStrLn handle msg

  return logChan

main :: IO ()
main = do
  Opt.Options {..} <- Opt.getArgs
  logChan <- startLogger stdout

  exists <- doesDirectoryExist root
  unless exists $ throw (RootDirectoryMissing root)

  let initial = initialState defaultConfig
      logFile = root </> "annotations.db"

  state <- if discard
    then freshLog initial logFile
    else openLog initial logFile >>= either (throw . LogError) return

  clients   <- atomically (newTVar M.empty)
  documents <- atomically (newTVar M.empty)


  atomically $ do
    config <- view #config <$> readCurrent state

    existing <- getImages state
    images <- unsafeIOToSTM (findNewImages config root existing)
    updateLog state (CmdImages images)

  print =<< atomically (readCurrent state)


  Warp.run 3000 $ serve (Proxy @ Api) (server $ Env {..})
