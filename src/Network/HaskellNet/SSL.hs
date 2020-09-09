{-# LANGUAGE CPP #-}
module Network.HaskellNet.SSL
  ( Settings (..)
  , defaultSettingsWithPort
  ) where

#if MIN_VERSION_network(3,0,0)
import Network.Socket (PortNumber)
#else
import Network.Socket.Internal (PortNumber)
#endif

data Settings = Settings
              { sslPort                        :: PortNumber
              , sslMaxLineLength               :: Int
              , sslLogToConsole                :: Bool
              , sslDisableCertificateValidation :: Bool
              } deriving(Eq, Ord, Show)

defaultSettingsWithPort :: PortNumber -> Settings
defaultSettingsWithPort p = Settings
  { sslPort = p
  , sslMaxLineLength = 10000
  , sslLogToConsole = False
  , sslDisableCertificateValidation = False
  }
