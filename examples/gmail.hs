{-# LANGUAGE OverloadedStrings #-}

import           Network.HaskellNet.IMAP.SSL
import           Network.HaskellNet.SMTP.SSL as SMTP

import           Network.HaskellNet.Auth (AuthType(LOGIN))

import qualified Data.ByteString.Char8 as B

username = "username@gmail.com"
password = "password"
recipient = "someone@somewhere.com"

imapTest = do
    c <- connectIMAPSSLWithSettings "imap.gmail.com" cfg
    login c username password
    mboxes <- list c
    mapM_ print mboxes
    select c "INBOX"
    msgs <- search c [ALLs]
    let firstMsg = head msgs
    msgContent <- fetch c firstMsg
    B.putStrLn msgContent
    logout c
  where cfg = defaultSettingsIMAPSSL { sslMaxLineLength = 100000 }

smtpTest = doSMTPSTARTTLS "smtp.gmail.com" $ \c -> do
    authSucceed <- SMTP.authenticate LOGIN username password c
    if authSucceed
      then sendPlainTextMail recipient username subject body c
      else print "Authentication error."
  where subject = "Test message"
        body    = "This is a test message"

main :: IO ()
main = smtpTest >> imapTest >> return ()
