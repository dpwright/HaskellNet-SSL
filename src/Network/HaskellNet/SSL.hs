module Network.HaskellNet.SSL
  ( Settings (..)
  , defaultSettingsWithPort
  ) where

import Network.Socket.Internal (PortNumber)

data Settings = Settings
              { sslPort                        :: PortNumber
              , sslMaxLineLength               :: Int
              , sslLogToConsole                :: Bool
              , sslDisableCertificateValidation :: Bool
              }

defaultSettingsWithPort :: PortNumber -> Settings
defaultSettingsWithPort p = Settings
  { sslPort = p
  , sslMaxLineLength = 10000
  , sslLogToConsole = False
  , sslDisableCertificateValidation = False
  }
