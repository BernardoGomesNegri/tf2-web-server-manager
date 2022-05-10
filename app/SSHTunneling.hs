module SSHTunneling where

import Control.Monad.Trans(lift)
import Control.Monad.Trans.Maybe(MaybeT(MaybeT), runMaybeT)
import System.IO(Handle)
import Control.Exception(catch)
import System.Process
import TransHelpers
import Network.Simple.TCP (HostName)

type Port = Int
type PID = Int

data Location = Location HostName Port

data SSHLogin = SSHLogin {
    loginLocation :: Location,
    loginUsername :: String
}

data SSHConn = SSHConn {
    connLocation :: Location,
    connPHandle :: ProcessHandle
}

-- We'll allocate ports in ascending order, the list will be in descending order.
-- Params: List of ports we can run, ssh binary path,
-- lower and upper limits on port numbers, SSH username,
-- remote host, port to connect on remote host, log handle.
setUpSsh :: [Port] -> Maybe String -> (Maybe Port, Maybe Port) -> SSHLogin -> Handle -> MaybeT IO SSHConn
setUpSsh list sshBinM (lower, upper) user sshdata = catch setup err
    where
        sshBin = maybe "ssh" id sshBinM
        setup = do
            createProcess (proc shhBin [
                ""
            ])