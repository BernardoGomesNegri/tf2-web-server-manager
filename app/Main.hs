{-#LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving#-}
module Main where
import Rcon
import Network.Wai.Middleware.RequestLogger(logStdoutDev)
import Network.Wai.Middleware.Static
import Network.HTTP.Types.Status(status403, status503, status200)
import Data.Default.Class(Default, def)
import Control.Concurrent.STM
    (atomically, TVar, newTVarIO, readTVarIO, modifyTVar')
import Control.Monad.Reader
    (MonadIO(..), MonadTrans(..), ReaderT(..), MonadReader(ask))
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class (liftIO)
import Web.Scotty.Trans
import System.Random(randomIO, randoms, getStdRandom, randomR)
import Data.Text.Lazy(Text, pack, append)

type Token = String
newtype State = State {unstate :: Map.Map Token Connection}

instance Default State where
    def = State Map.empty

newtype WebM a = WebM { runWebM :: ReaderT (TVar State) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar State))

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

gets :: (State -> b) -> WebM b
gets f = (ask >>= liftIO . readTVarIO) <&> f

modify :: (State -> State) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

main :: IO ()
main = do
    sync <- newTVarIO def
    let runActionToIO m = runReaderT (runWebM m) sync
    scottyT 3000 runActionToIO (web >> api)

api :: ScottyT Text WebM ()
api = do
    get "/api/validate/:token" $ do
#ifdef DEBUG
        text "1"
#else
        token <- param "token"
        tokenMap <- webM $ gets unstate
        case Map.lookup token tokenMap of
            Just _ -> text "1"
            _ -> text "0"
#endif
    
    post "/api/runcmd/:cmd" $ do
        cmd <- param "cmd"
        withToken (\token conn -> do
            liftIO $ print $ "running command: " ++ cmd ++ " with token " ++ token
            result <- liftIO $ sendCmd cmd conn
            case result of
                Just r -> do
                    status status200
                    text (pack r)
                Nothing -> do
                    status status503
                    text "error")

    get "/api/gettoken" $ do
        port <- param "port"
        adressName <- param "adress"
        pwd <- param "password"
        connM <- liftIO $ newConn port adressName pwd
        case connM of
            Right conn -> do
                randomToken <- liftIO $ randomString 25
                liftIO $ print $ "adding random key: " ++ randomToken
                webM $ modify $ \(State map) -> State $ Map.insert randomToken conn map
                text $ pack randomToken
            Left BadPassword -> do
                status status403
                text "wrong password"
            Left e -> do
                status status503
                text $ "error: " <> pack (show e)

withToken :: (Token -> Connection -> ActionT Text WebM ()) -> ActionT Text WebM ()
withToken f = do
    token <- param "token"
    tokenMap <- webM $ gets unstate
    case Map.lookup token tokenMap of
        Just conn -> f token conn
        Nothing -> status status403 >> text "invalid token"

web :: ScottyT Text WebM ()
web = do
#ifdef DEBUG
    middleware logStdoutDev
#endif
    middleware $ staticPolicy (noDots >-> isNotAbsolute >-> addBase "static")
    get "/server" (serveFile "frontend/server.html")
    get "/" (serveFile "frontend/login.html")

serveFile :: FilePath -> ActionT Text WebM ()
serveFile f = do
    htmlDoc <- liftIO $ readFile f
    html (pack htmlDoc)

randomString :: Int -> IO Token
randomString size =
    let all = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] in
    let randomChar = (!!) all <$> getStdRandom (randomR (0, 61)) in
    (sequenceA . replicate size) randomChar
