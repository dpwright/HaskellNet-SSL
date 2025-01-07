{-# LANGUAGE OverloadedStrings #-}

import           Network.HaskellNet.IMAP.SSL
import           Network.HaskellNet.SMTP.SSL as SMTP

import           Network.HaskellNet.Auth (AuthType(LOGIN), Password)
import           Network.Mail.Mime

import qualified Data.ByteString.Char8 as B
import           Data.String

username :: IsString s => s
username = "username@gmail.com"

password :: Password
password = "password"

recipient :: Address
recipient = "someone@somewhere.com"

imapTest :: IO ()
imapTest = do
    c <- connectIMAPSSLWithSettings "imap.gmail.com" cfg
    login c username password
    mboxes <- list c
    mapM_ print mboxes
    select c "INBOX"
    msgs@(firstMsg : _) <- search c [ALLs]
    msgContent <- fetch c firstMsg
    B.putStrLn msgContent
    logout c
  where cfg = defaultSettingsIMAPSSL { sslMaxLineLength = 100000 }

smtpTest :: IO ()
smtpTest = doSMTPSTARTTLS "smtp.gmail.com" $ \c -> do
    authSucceed <- SMTP.authenticate LOGIN username password c
    if authSucceed
      then do 
        mail <- simpleMail
          recipient
          username 
          subject
          body
          mempty
          mempty
        sendMail mail c -- recipient username subject body
      else print "Authentication error."
  where subject = "Test message"
        body    = "This is a test message"

main :: IO ()
main = do 
  smtpTest 
  imapTest
