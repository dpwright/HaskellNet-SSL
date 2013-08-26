module Network.HaskellNet.SSL ( connectSSL
                              , connectPlain
                              ) where

import Network.Connection
import Network.HaskellNet.BSStream
import Network.Socket.Internal (PortNumber)

import qualified Data.ByteString.Char8 as B
import Data.Default

type STARTTLS = IO ()

maxLineLength :: Int
maxLineLength = 10000

connectionGetBytes :: Connection -> Int -> IO B.ByteString
connectionGetBytes = loop B.empty where
  loop buf _ 0 = return buf
  loop buf c l = connectionGet c l >>= nextIteration
    where nextIteration b = loop (buf `B.append` b) c $ l - B.length b

connectionToStream :: Connection -> BSStream
connectionToStream c = BSStream
                       { bsGet = connectionGetBytes c
                       , bsPut = connectionPut c
                       , bsFlush = return ()
                       , bsClose = connectionClose c
                       , bsIsOpen = return True
                       , bsGetLine = connectionGetLine maxLineLength c
                       }

connectSSL :: String -> PortNumber -> IO BSStream
connectSSL hostname port = do
    c <- initConnectionContext >>= flip connectTo params
    return $ connectionToStream c
  where params = ConnectionParams hostname port (Just def) Nothing

connectPlain :: String -> PortNumber -> IO (BSStream, STARTTLS)
connectPlain hostname port = do
    ctx <- initConnectionContext
    c <- connectTo ctx params
    return (connectionToStream c, connectionSetSecure ctx c def)
  where params = ConnectionParams hostname port Nothing Nothing
