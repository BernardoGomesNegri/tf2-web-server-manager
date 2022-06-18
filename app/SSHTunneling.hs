module SSHTunneling where

import Control.Monad.Trans(lift)
import Control.Monad.Trans.Maybe(MaybeT(MaybeT))
import System.IO(hPrint, stderr)
import Control.Exception(catches, Handler(..), SomeException)
import Data.ByteString (ByteString)
import Control.Concurrent (forkIO)
import Network.Simple.TCP (HostName)
import Network.SSH.Client.LibSSH2.Errors as SSHErr(ErrorCode)
import Network.SSH.Client.LibSSH2 (directTcpIpEx, Channel, sessionInit, sessionClose)
import Network.SSH.Client.LibSSH2.Foreign (usernamePasswordAuth, writeChannel, readChannel)
import TransHelpers

type Port = Int
type PID = Int

data Msg = Send ByteString | Recv Int | Close

-- We keep two queues. One is for sending messages, one is for receiving data.
newtype SSHState =SSHState Channel

data Location = Location HostName Port

data SSHLogin = SSHLogin {
    loginLocation :: Location,
    loginUsername :: String,
    loginPassword :: String
}

data SSHConnection = SSHConn {
    sshConnState :: SSHState,
    sshConnWrite :: ByteString -> IO (),
    sshConnRecv :: Int -> IO ByteString,
    sshConnClose :: IO ()
}

-- Helper, equivalent of "libssh2_channel_direct_tcpip" on the C library
directTcpIp s host port = directTcpIpEx s host port "127.0.0.1" 22

-- Params: The login and location of the SSH server, the location
-- and port of the TF2 server
setUpSsh :: SSHLogin -> String -> Port -> MaybeT IO SSHConnection
setUpSsh SSHLogin {loginLocation = (Location host port), loginUsername = user,
         loginPassword = password} destHost destPort =
    (MaybeT . handler . fmap pure) $ do
        session <- sessionInit host port
        usernamePasswordAuth session user password
        chan <- directTcpIp session destHost destPort
        pure $ SSHConn {
            sshConnState = SSHState chan,
            sshConnWrite = writeChannel chan,
            sshConnRecv = readChannel chan . fromIntegral,
            sshConnClose = sessionClose session
        }
    where
        handler :: IO (Maybe a) -> IO (Maybe a)
        handler io = catches io [Handler sshHandler, Handler other]

        sshHandler :: SSHErr.ErrorCode -> IO (Maybe a)
        sshHandler err = do
            hPrint stderr err
            pure Nothing

        other :: SomeException -> IO (Maybe a)
        other _ = pure Nothing
