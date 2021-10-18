module Rcon where
import Network.Simple.TCP
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as UTF8
import Data.Int as Int
import Data.Binary
import Control.Exception (handle, SomeException (SomeException))
import Control.Concurrent(threadDelay)

type Port = Int

data RequestType = Auth | Command | AuthResponse | CommandResponse deriving Show

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

data ConnErr = BadPassword | ConnError deriving Show

newConn :: Port -> HostName -> String -> IO (Either ConnErr Connection)
newConn port adress pwd =
    handle errHandler $
        connect adress (show port) $ \(socket,addr) -> do
            send socket (createPackage pwd 1293128 Auth)
            response <- getServerPacket socket Nothing
            case response of
                Just r -> do
                    newResM <- getServerPacket socket Nothing
                    case newResM of
                        Just newRes ->
                            if packetId newRes /= -1 then
                                return $ return (Connection {port = port, adress = adress, password = pwd})
                            else return $ Left BadPassword
                        Nothing -> return $ Left ConnError
                Nothing -> return $ Left ConnError

    where
        errHandler :: SomeException -> IO (Either ConnErr Connection)
        errHandler _ = return $ Left ConnError

getServerPacket :: Socket -> Maybe Int32 -> IO (Maybe Packet)
getServerPacket socket idM = do
    let recsA s count = do
            stuff <- recv socket s
            case stuff of
                Just x -> return $ Just x
                Nothing ->
                    if count < 6 then threadDelay 100000 >> recsA s (count + 1) else return Nothing
    let recs = (`recsA` 0)
    sM <- recs 4
    case sM of
        Just s -> do
            let size = decode (BL.fromStrict s)
            restM <- recs size
            case restM of
                Just rest -> do
                    let id = decodeS $ B.take 4 rest :: Int32
                    let reqType = (intToRequestReceive . decodeS . B.take 4 . snd . B.splitAt 4) rest
                    let body = (fmap decodeS . takeUntilDoubleNullExt . snd . B.splitAt 8) rest
                    return $ Packet (fromIntegral size) id <$> reqType <*> body
                Nothing -> return Nothing
        Nothing -> return Nothing
    
    where
        takeUntilDoubleNullExt bs = takeUntilDoubleNull bs bs
        takeUntilDoubleNull bs total =
            let isDoubleNull = ((== "\0\0") . UTF8.toString . B.take 2) bs in
            if B.length bs < 2 then
                if isDoubleNull then return $ lastX 2 total else takeUntilDoubleNull (snd $ B.splitAt 1 bs) total
            else Nothing
        lastX num bs =
            B.take (B.length bs - num) bs
        decodeS :: Binary a => B.ByteString -> a
        decodeS = decode . BL.fromStrict

createPackage :: String -> Int32 -> RequestType -> B.ByteString
createPackage command idInt reqType =
  B.concat [encodeS sizeInt, id, reqTypeInt, body, null]
  where
    sizeInt :: Int32
    sizeInt = fromIntegral (B.length $ UTF8.fromString command) + (10 :: Int32)
    id = encodeS idInt
    reqTypeInt = encodeS $ requestToInt reqType
    body = UTF8.fromString command
    null = UTF8.fromString "\0\0"
    encodeS :: Binary a => a -> B.ByteString
    encodeS = BL.toStrict . encode