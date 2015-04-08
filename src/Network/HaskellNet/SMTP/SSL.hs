module Network.HaskellNet.SMTP.SSL
    ( -- * Establishing connection
      connectSMTPSSL
    , connectSMTPSSLWithSettings
    , connectSMTPSTARTTLS
    , connectSMTPSTARTTLSWithSettings
      -- * Other Useful Operations
    , doSMTPSSL
    , doSMTPSSLWithSettings
    , doSMTPSTARTTLS
    , doSMTPSTARTTLSWithSettings
      -- * Settings
    , Settings(..)
    , defaultSettingsSMTPSSL
    , defaultSettingsSMTPSTARTTLS
      -- * Network.HaskellNet.SMTP re-exports
    , module Network.HaskellNet.SMTP
    ) where

import Network.HaskellNet.SMTP
import Network.HaskellNet.SSL

import Network.HaskellNet.SSL.Internal

import Network.HaskellNet.BSStream
import Network.BSD (getHostName)

import qualified Data.ByteString.Char8 as B

import Control.Exception
import Control.Monad
import Data.IORef

connectSMTPSSL :: String -> IO SMTPConnection
connectSMTPSSL hostname = connectSMTPSSLWithSettings hostname defaultSettingsSMTPSSL

connectSMTPSSLWithSettings :: String -> Settings -> IO SMTPConnection
connectSMTPSSLWithSettings hostname cfg = connectSSL hostname cfg >>= connectStream

connectSMTPSTARTTLS :: String -> IO SMTPConnection
connectSMTPSTARTTLS hostname = connectSMTPSTARTTLSWithSettings hostname defaultSettingsSMTPSTARTTLS

connectSMTPSTARTTLSWithSettings :: String -> Settings -> IO SMTPConnection
connectSMTPSTARTTLSWithSettings hostname cfg = connectSTARTTLS hostname cfg >>= connectStream

connectSTARTTLS :: String -> Settings -> IO BSStream
connectSTARTTLS hostname cfg = do
    (bs, startTLS) <- connectPlain hostname cfg

    greeting <- bsGetLine bs
    failIfNot bs 220 $ parseResponse greeting

    hn <- getHostName
    bsPut bs $ B.pack ("HELO " ++ hn ++ "\r\n")
    getResponse bs >>= failIfNot bs 250
    bsPut bs $ B.pack "STARTTLS\r\n"
    getResponse bs >>= failIfNot bs 220

    startTLS

    prefixRef <- newIORef [greeting]
    return $ bs {bsGetLine = prefixedGetLine prefixRef (bsGetLine bs)}
  where parseResponse = parse . B.unpack
        parse s = (getCode  s, s)
        getCode = read . head . words
        getResponse bs = liftM parseResponse $ bsGetLine bs

failIfNot :: BSStream -> Integer -> (Integer, String) -> IO ()
failIfNot bs code (rc, rs) = when (code /= rc) closeAndFail
  where closeAndFail = bsClose bs >> fail ("cannot connect to server: " ++ rs)

-- This is a bit of a nasty hack.  Network.HaskellNet.SMTP.connectStream
-- expects to receive a status 220 from the server as soon as it connects,
-- but we've intercepted it in order to establish a STARTTLS connection.
-- This allows us to keep hold of the original greeting and pass it back to
-- HaskellNet.
prefixedGetLine :: IORef [B.ByteString] -> IO B.ByteString -> IO B.ByteString
prefixedGetLine prefix rawGetLine = readIORef prefix >>= deliverLine
  where deliverLine [] = rawGetLine
        deliverLine (l:ls) = writeIORef prefix ls >> return l

bracketSMTP :: IO SMTPConnection -> (SMTPConnection -> IO a) -> IO a
bracketSMTP = flip bracket closeSMTP

doSMTPSSL :: String -> (SMTPConnection -> IO a) -> IO a
doSMTPSSL host = bracketSMTP $ connectSMTPSSL host

doSMTPSSLWithSettings :: String -> Settings -> (SMTPConnection -> IO a) -> IO a
doSMTPSSLWithSettings host port = bracketSMTP $ connectSMTPSSLWithSettings host port

doSMTPSTARTTLS :: String -> (SMTPConnection -> IO a) -> IO a
doSMTPSTARTTLS host = bracketSMTP $ connectSMTPSTARTTLS host

doSMTPSTARTTLSWithSettings :: String -> Settings -> (SMTPConnection -> IO a) -> IO a
doSMTPSTARTTLSWithSettings host port = bracketSMTP $ connectSMTPSTARTTLSWithSettings host port

defaultSettingsSMTPSSL :: Settings
defaultSettingsSMTPSSL = defaultSettingsWithPort 465

defaultSettingsSMTPSTARTTLS :: Settings
defaultSettingsSMTPSTARTTLS = defaultSettingsWithPort 587
