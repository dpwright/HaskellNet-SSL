module Network.HaskellNet.SMTP.SSL
    ( connectSMTPSSL
    , connectSMTPSSLPort
    , connectSMTPSTARTTLS
    , connectSMTPSTARTTLSPort
    ) where

import Network.Socket.Internal (PortNumber)
import Network.HaskellNet.SMTP
import Network.HaskellNet.SSL

import Network.HaskellNet.BSStream
import Network.BSD (getHostName)

import qualified Data.ByteString.Char8 as B

import Control.Monad
import Data.IORef

connectSMTPSSL :: String -> IO SMTPConnection
connectSMTPSSL hostname = connectSMTPSSLPort hostname 465

connectSMTPSSLPort :: String -> PortNumber -> IO SMTPConnection
connectSMTPSSLPort hostname port = connectSSL hostname port >>= connectStream

connectSMTPSTARTTLS :: String -> IO SMTPConnection
connectSMTPSTARTTLS hostname = connectSMTPSTARTTLSPort hostname 587

connectSMTPSTARTTLSPort :: String -> PortNumber -> IO SMTPConnection
connectSMTPSTARTTLSPort hostname port = connectSTARTTLS hostname port >>= connectStream

connectSTARTTLS :: String -> PortNumber -> IO BSStream
connectSTARTTLS hostname port = do
    (bs, startTLS) <- connectPlain hostname port

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
