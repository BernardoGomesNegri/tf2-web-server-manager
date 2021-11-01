{-#LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveGeneric#-}
module Main where
import Rcon
import Network.Wai.Middleware.RequestLogger(logStdoutDev)
import Network.Wai.Middleware.Static hiding ((<|>))
import Text.Parsec hiding (State)
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
import Text.Read (readMaybe)
import System.Environment (getArgs)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

type Token = String
newtype State = State {unstate :: Map.Map Token Connection}

instance Default State where
    def = State Map.empty

newtype WebM a = WebM { runWebM :: ReaderT (TVar State) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar State))

type Seconds = Int

data Player = Player {
    userid :: Int,
    name :: String,
    steamid :: String,
    time :: Seconds,
    ping :: Int,
    loss :: Int,
    connectionStatus :: String,
    playerAdress :: String
} deriving (Generic, Show)

instance ToJSON Player where

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
    args <- getArgs
    let port = safeHead args >>= readMaybe :: Maybe Int
    case port of
        Just p -> scottyT p runActionToIO (web >> api)
        Nothing -> scottyT 3000 runActionToIO (web >> api)

safeHead [] = Nothing
safeHead (x:_) = Just x

api :: ScottyT Text WebM ()
api = do
    get "/api/validate/:token" $ do
        token <- param "token"
        tokenMap <- webM $ gets unstate
        case Map.lookup token tokenMap of
            Just _ -> text "1"
            _ -> text "0"

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

    get "/api/getplayers" $ do
        withToken $ \token conn -> do
            result <- liftIO $ sendCmd "status" conn
            case result of
                Just r -> do
                    let players = drop 10 . lines $ r
                    let playerList = traverse (parse parsePlayer "") players
                    case playerList of
                        Right l -> json l
                        Left e -> status status503 >> text (pack $ show e)
                Nothing -> status status503 >> text "error"
    
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> isNotAbsolute >-> addBase "static")

parseUntilX :: Char -> Parsec String () Char -> Parsec String () String
parseUntilX c p = go ""
    where
    go :: String -> Parsec String () String
    go s = (do
        char <- p
        if char == c then
            return s
        else go $ s ++ [char]) <|> return s

parseUntilXconsume c p = go ""
    where
    go :: String -> Parsec String () String
    go s = (do
        char <- p
        if char == c then
            return s
        else go $ s ++ [char]) <|> (char c >> return s)

parseUntilEof :: Parsec String () String
parseUntilEof = go ""
    where
    go :: String -> Parsec String () String
    go s =
        (try eof >> return s) <|> (do
            ch <- anyChar
            go (s <> [ch]))

parsePlayer :: Parsec String () Player
parsePlayer = do
    let anyNum = do
            numStr <- parseUntilX ' ' (choice (char <$> ['0'..'9']))
            case readMaybe numStr of
                Just n -> return n
                Nothing -> fail $ "failed to parse number: " <> numStr
    let anyNumSep sep = do
            numStr <- parseUntilXconsume sep (choice (char <$> ['0'..'9']))
            case readMaybe numStr of
                Just n -> return n
                Nothing -> fail $ "failed to parse number sep: " <> numStr
    let white = many (char ' ')
    char '#'
    white
    numId <- anyNum
    white
    name <- between (char '\"') (char '\"') (many1 $ noneOf "\"")
    white
    steamid <- parseUntilX ' ' anyChar
    white
    minutes <- anyNumSep ':' :: Parsec String () Int
    seconds <- anyNum
    white
    let time = (minutes * 60) + seconds
    ping <- anyNum
    white
    loss <- anyNum
    white
    connState <- parseUntilX ' ' anyChar
    white
    adr <- parseUntilEof
    return $ Player {name = name, steamid = steamid, ping = ping, loss = loss, connectionStatus = connState,
        playerAdress = adr, time = time, userid = numId}

withToken :: (Token -> Connection -> ActionT Text WebM ()) -> ActionT Text WebM ()
withToken f = do
    token <- param "token"
    tokenMap <- webM $ gets unstate
    case Map.lookup token tokenMap of
        Just conn -> f token conn
        Nothing -> status status403 >> text "invalid token"

web :: ScottyT Text WebM ()
web = do
    get "/server" (serveFile "frontend/server.html")
    get "/" (serveFile "frontend/login.html")

serveFile :: FilePath -> ActionT Text WebM ()
serveFile f = do
    htmlDoc <- liftIO $ readFile f
    html (pack htmlDoc)

randomString :: Int -> IO Token
randomString size =
    let all = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] in
    let randomChar = (!!) all <$> getStdRandom (randomR (0, length all - 1)) in
    (sequenceA . replicate size) randomChar
