module Network.HaskellNet.SSL
  ( Settings (..)
  , defaultSettingsWithPort
  , connectSSL
  , connectPlain
  ) where

import Network.Connection
import Network.HaskellNet.BSStream
import Network.Socket.Internal (PortNumber)

import qualified Data.ByteString.Char8 as B
import Data.Default

type STARTTLS = IO ()

data Settings = Settings
              { sslPort          :: PortNumber
              , sslMaxLineLength :: Int
              }

defaultSettingsWithPort :: PortNumber -> Settings
defaultSettingsWithPort p = Settings
  { sslPort = p
  , sslMaxLineLength = 10000
  }

connectionGetBytes :: Connection -> Int -> IO B.ByteString
connectionGetBytes = loop B.empty where
  loop buf _ 0 = return buf
  loop buf c l = connectionGet c l >>= nextIteration
    where nextIteration b = loop (buf `B.append` b) c $ l - B.length b

connectionToStream :: Connection -> Settings -> BSStream
connectionToStream c cfg = BSStream
  { bsGet = connectionGetBytes c
  , bsPut = connectionPut c
  , bsFlush = return ()
  , bsClose = connectionClose c
  , bsIsOpen = return True
  , bsGetLine = connectionGetLine maxl c
  } where maxl = sslMaxLineLength cfg

connectSSL :: String -> Settings -> IO BSStream
connectSSL hostname cfg = do
    c <- initConnectionContext >>= flip connectTo params
    return $ connectionToStream c cfg
  where params = ConnectionParams hostname port (Just def) Nothing
        port = sslPort cfg

connectPlain :: String -> Settings -> IO (BSStream, STARTTLS)
connectPlain hostname cfg = do
    ctx <- initConnectionContext
    c <- connectTo ctx params
    return (connectionToStream c cfg, connectionSetSecure ctx c def)
  where params = ConnectionParams hostname port Nothing Nothing
        port = sslPort cfg
