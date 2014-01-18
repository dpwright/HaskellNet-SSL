module Network.HaskellNet.SSL
  ( Settings (..)
  , defaultSettingsWithPort
  ) where

import Network.Socket.Internal (PortNumber)

data Settings = Settings
              { sslPort          :: PortNumber
              , sslMaxLineLength :: Int
              }

defaultSettingsWithPort :: PortNumber -> Settings
defaultSettingsWithPort p = Settings
  { sslPort = p
  , sslMaxLineLength = 10000
  }
