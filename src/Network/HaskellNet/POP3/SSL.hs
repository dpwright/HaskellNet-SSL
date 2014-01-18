module Network.HaskellNet.POP3.SSL
    ( -- * Establishing connection
      connectPop3SSL
    , connectPop3SSLWithSettings
    ) where

import Network.HaskellNet.POP3.Connection
import Network.HaskellNet.POP3
import Network.HaskellNet.SSL

import Network.HaskellNet.SSL.Internal

connectPop3SSL :: String -> IO POP3Connection
connectPop3SSL hostname = connectPop3SSLWithSettings hostname cfg
  where cfg = defaultSettingsWithPort 995

connectPop3SSLWithSettings :: String -> Settings -> IO POP3Connection
connectPop3SSLWithSettings hostname cfg = connectSSL hostname cfg >>= connectStream
