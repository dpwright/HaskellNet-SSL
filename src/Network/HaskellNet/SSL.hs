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

-- | Settings for configuring HaskellNet connections
data Settings = Settings
              { sslPort                        :: PortNumber -- ^ Port number to connect to
              , sslMaxLineLength               :: Int -- ^ Max line lengths
              , sslLogToConsole                :: Bool -- ^ Log info to console
              , sslDisableCertificateValidation :: Bool -- ^ Disable certificate validation
              } deriving(Eq, Ord, Show)

-- | Construct default settings for a port
defaultSettingsWithPort :: PortNumber -> Settings
defaultSettingsWithPort p = Settings
  { sslPort = p
  , sslMaxLineLength = 10000
  , sslLogToConsole = False
  , sslDisableCertificateValidation = False
  }
