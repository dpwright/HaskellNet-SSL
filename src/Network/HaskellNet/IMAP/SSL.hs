-- | IMAP SSL Connections
module Network.HaskellNet.IMAP.SSL
    ( -- * Establishing connection
      connectIMAPSSL
    , connectIMAPSSLWithSettings
      -- * Settings
    , Settings(..)
    , defaultSettingsIMAPSSL
      -- * Network.HaskellNet.IMAP re-exports
    , module Network.HaskellNet.IMAP
    ) where

import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP
import Network.HaskellNet.SSL

import Network.HaskellNet.SSL.Internal

-- | Create IMAP connection with default settings
connectIMAPSSL :: String -> IO IMAPConnection
connectIMAPSSL hostname = connectIMAPSSLWithSettings hostname defaultSettingsIMAPSSL

-- | Create IMAP connection with given settings
connectIMAPSSLWithSettings :: String -> Settings -> IO IMAPConnection
connectIMAPSSLWithSettings hostname cfg = connectSSL hostname cfg >>= connectStream

-- | Default IMAP SSL settings, port 993
defaultSettingsIMAPSSL :: Settings
defaultSettingsIMAPSSL = defaultSettingsWithPort 993
