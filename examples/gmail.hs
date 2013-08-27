{-# LANGUAGE OverloadedStrings #-}

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.SSL

import Network.HaskellNet.SMTP
import Network.HaskellNet.SMTP.SSL

import Network.HaskellNet.Auth (AuthType(PLAIN))

import Network.BSD (getHostName)
import qualified Data.ByteString.Char8 as B

username = "username@gmail.com"
password = "password"
recipient = "someone@somewhere.com"

imapTest = do
    c <- connectIMAPSSL "imap.gmail.com"
    login c username password
    mboxes <- list c
    mapM_ print mboxes
    select c "INBOX"
    msgs <- search c [ALLs]
    let firstMsg = head msgs
    msgContent <- fetch c firstMsg
    B.putStrLn msgContent
    logout c

smtpTest = do
    c <- connectSMTPSTARTTLS "smtp.gmail.com"
    sendCommand c $ AUTH PLAIN username password
    sendMail username [recipient] mailContent c
    closeSMTP c
  where mailContent = subject `B.append` body
        subject = "Subject: Test message\r\n\r\n"
        body = "This is a test message"

main :: IO ()
main = smtpTest >> imapTest >> return ()
