{-#LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving#-}
module Main where
import Rcon
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse
import Network.HTTP.Types.Status(status403, status503, status200)
import Data.Default.Class
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Functor
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class (liftIO)
import Web.Scotty.Trans
import Data.Text.Lazy(Text, pack)

newtype State = State {unstate :: Map.Map String Connection}

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
    scottyT 3000 runActionToIO app

app :: ScottyT Text WebM ()
app = do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> isNotAbsolute >-> addBase "static")

    post (capture "api/runcmd/:cmd") $ do
        cmd <- param "cmd"
        token <- param "token"
        tokenMap <- webM $ gets unstate
        case Map.lookup token tokenMap of
            Just conn -> do
                result <- liftIO $ sendCmd cmd conn
                case result of
                    Just r -> do
                        status status200
                        text (pack r)
                    Nothing -> do
                        status status503
                        text "error on running command"
            Nothing -> do
                status status403
                text "invalidtoken"