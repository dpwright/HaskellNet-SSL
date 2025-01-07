# HaskellNet-SSL

[![haskell ci](https://github.com/dpwright/HaskellNet-SSL/actions/workflows/haskell.yml/badge.svg)](https://github.com/dpwright/HaskellNet-SSL/actions/workflows/haskell.yml)

This package ties together the excellent [HaskellNet][HaskellNet] and
[crypton-connection][crypton-connection] packages to make it easy to open IMAP and SMTP
connections over SSL.  This is a simple "glue" library; all credit for a)
connecting to IMAP/SMTP servers and b) making an SSL connection goes to the
aforementioned libraries.

[HaskellNet]: https://github.com/jtdaugherty/HaskellNet
[crypton-connection]: https://github.com/kazu-yamamoto/crypton-connection
