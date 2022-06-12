module SSHTunneling where

import Control.Monad.Trans(lift)
import Control.Monad.Trans.Maybe(MaybeT(MaybeT), runMaybeT)
import System.IO(Handle)
import Control.Exception(handle)
import Data.ByteString (ByteString)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TQueue
import Network.Simple.TCP (HostName)
import Network.SSH.Client.LibSSH2 (withSession)
import Network.SSH.Client.LibSSH2.Foreign (usernamePasswordAuth)
import TransHelpers

type Port = Int
type PID = Int

data Msg = Send ByteString | Recv Int | Close

-- We keep two queues. One is for sending messages, one is for receiving data.
data SSHState = SSHState {
    sshStateInQueue :: TQueue Msg,
    sshStateOutQueue :: TQueue ByteString
}

data Location = Location HostName Port

data SSHLogin = SSHLogin {
    loginLocation :: Location,
    loginUsername :: String,
    loginPassword :: String
}

data SSHConnection = SSHConn {
    sshConnState :: SSHState,
    sshConnWrite :: ByteString -> IO (),
    sshConnRecv :: Int -> IO ByteString
}

-- Params: The login and location of the SSH server, the location
-- and port of the TF2 server
setUpSsh :: SSHLogin -> String -> Port -> MaybeT IO SSHConnection
setUpSsh SSHLogin {loginLocation = (Location host port), loginUsername = user,
         loginPassword = password} destHost destPort =
    (lift . handle err) $ do
        withSession host port $ \session -> do
            usernamePasswordAuth session user password
            chan <- directTcpIpEx destHost destPort "127.0.0.1" 22
            forkIO ()
    where
        
