module Network.HaskellNet.IMAP.SSL
    ( -- * Establishing connection
      connectIMAPSSL
    , connectIMAPSSLWithSettings
    ) where

import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP
import Network.HaskellNet.SSL

connectIMAPSSL :: String -> IO IMAPConnection
connectIMAPSSL hostname = connectIMAPSSLWithSettings hostname cfg
  where cfg = defaultSettingsWithPort 993

connectIMAPSSLWithSettings :: String -> Settings -> IO IMAPConnection
connectIMAPSSLWithSettings hostname cfg = connectSSL hostname cfg >>= connectStream
