module Network.HaskellNet.IMAP.SSL
    ( -- * Establishing connection
      connectIMAPSSL
    , connectIMAPSSLPort
    ) where

import Network.Socket.Internal (PortNumber)
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP
import Network.HaskellNet.SSL

connectIMAPSSL :: String -> IO IMAPConnection
connectIMAPSSL hostname = connectIMAPSSLPort hostname 993

connectIMAPSSLPort :: String -> PortNumber -> IO IMAPConnection
connectIMAPSSLPort hostname port = connectSSL hostname cfg >>= connectStream
  where cfg = defaultSettingsWithPort port
