module Rcon(RequestType(..), Connection, ConnErr(..), sendCmd, newConn, withConn) where
import Network.Simple.TCP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as UTF8
import Data.Int as Int
import Data.Binary
import LeEncoding
import Control.Exception (handle, SomeException (SomeException))
import Control.Monad.Trans(lift)
import Control.Monad.Trans.Maybe(MaybeT(MaybeT), runMaybeT)
import Control.Concurrent(threadDelay)
import System.Random(randomIO)
import TransHelpers
import Control.Monad (join)

-- Everything about the RCON protocol is documented at https://developer.valvesoftware.com/wiki/Source_RCON_Protocol

individualWait = 10000
numberWait = 50

type Port = Int

data RequestType = Auth | Command | AuthResponse | CommandResponse deriving Show

data Connection = Connection {
    port :: Port,
    adress :: HostName,
    password :: String
} deriving Show

data Packet = Packet {
    size :: Int32,
    packetId :: Int32,
    reqType :: RequestType,
    body :: String
} deriving Show

data ConnErr = BadPassword | UnexpectedResponse | ConnError String deriving Show

errHandler :: SomeException -> IO (Either ConnErr Connection)
errHandler e = return $ Left $ ConnError (show e)

errHandlerMaybe :: SomeException -> IO (Maybe a)
errHandlerMaybe _ = return Nothing

withConn :: Connection -> (Socket -> IO a) -> MaybeT IO a
withConn conn@Connection {adress = adress, port = port, password = pwd} action = do
    MaybeT $
        handle errHandlerMaybe $
            connect adress (show port) $ \(socket,_) -> do
                authenticated <- authenticateConn conn socket
                if authenticated then do
                    result <- action socket
                    return $ return result
                else return Nothing


requestToInt :: RequestType -> Int32
requestToInt req =
    case req of
        Auth -> (3 :: Int32)
        Command -> (2 :: Int32)
        AuthResponse -> (2 :: Int32)
        CommandResponse -> (0 :: Int32)

intToRequestSend :: Int32 -> Maybe RequestType
intToRequestSend 0 = return CommandResponse
intToRequestSend 2 = return Command
intToRequestSend 3 = return Auth
intToRequestSend _ = Nothing

intToRequestReceive :: Int32 -> Maybe RequestType
intToRequestReceive 0 = return CommandResponse
intToRequestReceive 2 = return AuthResponse
intToRequestReceive 3 = return Auth
intToRequestReceive _ = Nothing

newConn :: Port -> HostName -> String -> IO (Either ConnErr Connection)
newConn port adress pwd =
    handle errHandler $
        connect adress (show port) $ \(socket,addr) -> do
            authenticated <- authenticateConn (Connection port adress pwd) socket
            if authenticated then
                return $ return (Connection port adress pwd)
            else return $ Left BadPassword

authenticateConn :: Connection -> Socket -> IO Bool
authenticateConn Connection { port = port, adress = adress, password = pwd} socket = do
    send socket (createPackage pwd 123123 Auth)
    response <- runMaybeT $ getServerPacket socket Nothing
    case response of
        Just _ -> do
            newResM <- runMaybeT $ getServerPacket socket Nothing
            case newResM of
                Just newRes ->
                    if packetId newRes /= -1 then
                        return True
                    else return False
                Nothing -> return False
        Nothing -> return False

getAndWait :: Socket -> Int -> MaybeT IO B.ByteString
getAndWait socket size = MaybeT $ go 0 B.empty where
    go :: Int -> B.ByteString -> IO (Maybe B.ByteString)
    go count total = do
        stuff <- recv socket size
        case stuff of
            Just x -> do
                let newTotal = B.concat [total,x]
                if B.length x < size && count < numberWait then threadDelay individualWait >> go (count + 1) newTotal else
                    return $ return newTotal
            Nothing ->
                if count < numberWait then threadDelay individualWait >> go (count + 1) total else return Nothing

getServerPacket :: Socket -> Maybe Int32 -> MaybeT IO Packet
getServerPacket socket idM = do
    let recs = getAndWait socket
    s <- recs 4
    let size = intDecode s
    rest <- recs (fromIntegral size)
    let id = intDecode $ B.take 4 rest :: Int32
    let toDo = do
            let reqType = (intToRequestReceive . intDecode . B.take 4 . snd . B.splitAt 4) rest
            let body = (UTF8.toString . takeUntilDoubleNull . snd . B.splitAt 8) rest
            Packet (fromIntegral size) id <$> reqType <*> return body
    case idM of
        Just desiredId ->
            if id /= desiredId then
                hoistMaybe Nothing
            else
                hoistMaybe toDo
        Nothing -> hoistMaybe toDo

    where
        takeUntilDoubleNull bs = B.take (B.length bs - 2) bs

createPackage :: String -> Int32 -> RequestType -> B.ByteString
createPackage command idInt reqType =
    B.concat [intEncode sizeInt, id, reqTypeInt, body, null]
    where
        sizeInt :: Int32
        sizeInt = fromIntegral (B.length $ UTF8.fromString command) + (10 :: Int32)
        id = intEncode idInt
        reqTypeInt = intEncode $ requestToInt reqType
        body = UTF8.fromString command
        null = UTF8.fromString "\0\0"

sendCmd :: String -> Connection -> IO (Maybe String)
-- We need to trim off the first line because the input comes like this:
-- command
-- command output
sendCmd s c = runMaybeT $ unlines . safeInit . lines <$> sendCmdInternal s c

safeInit [] = []
safeInit xs = init xs

sendCmdInternal :: String -> Connection -> MaybeT IO String
sendCmdInternal body conn =
    MaybeT $ join <$> runMaybeT (
    withConn conn $ \socket -> do
        id <- randomIO
        let package = createPackage body id Command
        -- The reason we create a dummy packet is because when a packet response exceeds
        -- 4096 bytes, the server will send two responses.
        -- By sending a dummy package that should not be valid,
        -- the server will simply mirror the dummy packet back to us,
        -- but only after it has already sent the response for the first packet
        -- See more at https://developer.valvesoftware.com/wiki/Source_RCON_Protocol#Multiple-packet_Responses
        let dummyPackage = createPackage "" 0 CommandResponse
        send socket package
        send socket dummyPackage
        print $ "sent command: " ++ body ++ " to server"
        let go total = do
                response <- runMaybeT $ getServerPacket socket (Just id)
                case response of
                        Just Packet {reqType = reqType, body = body} ->
                            case reqType of
                                CommandResponse -> do
                                    if body == "" then return $ return (total ++ body) else go (total ++ body)
                                _ -> return Nothing
                        _ -> return $ Just (total ++ body)
        go "")
