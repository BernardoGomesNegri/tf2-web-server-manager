module Rcon where
import Network.Simple.TCP
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as UTF8
import Data.Int as Int
import Data.Binary
import LeEncoding
import Control.Exception (handle, SomeException (SomeException))
import Control.Concurrent(threadDelay)
import System.Random(randomIO)
import Control.Monad (join)

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

withConn :: Connection -> (Socket -> IO a) -> IO (Maybe a)
withConn conn@Connection {adress = adress, port = port, password = pwd} action = do
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
    where
        errHandler :: SomeException -> IO (Either ConnErr Connection)
        errHandler e = return $ Left $ ConnError (show e)

authenticateConn :: Connection -> Socket -> IO Bool
authenticateConn Connection { port = port, adress = adress, password = pwd} socket = do
    send socket (createPackage pwd 123123 Auth)
    response <- getServerPacket socket Nothing
    case response of
        Just r -> do
            newResM <- getServerPacket socket Nothing
            case newResM of
                Just newRes ->
                    if packetId newRes /= -1 then
                        return True
                    else return False
                Nothing -> return False
        Nothing -> return False

getServerPacket :: Socket -> Maybe Int32 -> IO (Maybe Packet)
getServerPacket socket idM = do
    let go count total s = do
            stuff <- recv socket s
            case stuff of
                Just x -> do
                    let newTotal = B.concat [total,x]
                    if B.length x < s && count < 6 then threadDelay 100000 >> go (count + 1) newTotal s else
                        return $ return newTotal
                Nothing ->
                    if count < 6 then threadDelay 100000 >> go (count + 1) total s else return Nothing
    let recs = go 0 B.empty
    sM <- recs 4
    case sM of
        Just s -> do
            let size = intDecode s
            restM <- recs (fromIntegral size)
            case restM of
                Just rest -> do
                    let id = intDecode $ B.take 4 rest :: Int32
                    let toDo = do
                            let reqType = (intToRequestReceive . intDecode . B.take 4 . snd . B.splitAt 4) rest
                            let body = (UTF8.toString . takeUntilDoubleNull . snd . B.splitAt 8) rest
                            case Packet (fromIntegral size) id <$> reqType <*> return body of
                                Just s -> return $ return s
                                Nothing -> return Nothing
                    case idM of
                        Just desiredId ->
                            if id /= desiredId then
                                return Nothing
                            else
                                toDo
                        Nothing -> toDo
                Nothing -> return Nothing
        Nothing -> return Nothing

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
sendCmd body conn =
    join <$>
    withConn conn (\socket -> do
        id <- randomIO
        let package = createPackage body id Command
        let dummyPackage = createPackage "" 0 CommandResponse
        send socket package
        send socket dummyPackage
        let go total = do
                response <- getServerPacket socket (Just id)
                let result = (case response of
                        Just Packet {reqType = reqType, body = body} ->
                            case reqType of
                                CommandResponse -> do
                                    if body == "" then return $ return (total ++ body) else go (total ++ body)
                                _ -> return Nothing
                        _ -> return $ Just (total ++ body))
                result
        go "")
