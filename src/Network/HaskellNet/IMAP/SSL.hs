module Network.HaskellNet.IMAP.SSL
    ( -- * Establishing connection
      connectIMAPSSL
    , connectIMAPSSLWithSettings
      -- * Settings
    , defaultSettingsIMAPSSL
    ) where

import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP
import Network.HaskellNet.SSL

import Network.HaskellNet.SSL.Internal

connectIMAPSSL :: String -> IO IMAPConnection
connectIMAPSSL hostname = connectIMAPSSLWithSettings hostname defaultSettingsIMAPSSL

connectIMAPSSLWithSettings :: String -> Settings -> IO IMAPConnection
connectIMAPSSLWithSettings hostname cfg = connectSSL hostname cfg >>= connectStream

defaultSettingsIMAPSSL :: Settings
defaultSettingsIMAPSSL = defaultSettingsWithPort 993
