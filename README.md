# Scotty-TLS

You can test by generating a self-signed certificate like this:

```sh
openssl req -nodes -newkey rsa:2048 -keyout example.key -out example.csr \
    -subj "/C=GB/ST=London/L=London/O=Acme Widgets/OU=IT Department/CN=example.com"
openssl x509 -req -days 365 -in example.csr -signkey example.key -out example.crt
```
For more details on making certificates, see [this guide](http://www.akadia.com/services/ssh_test_certificate.html).


Install and run with:

```text
cabal update && cabal install scotty-tls
```

```haskell
{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid    (mconcat)
import           Web.Scotty
import           Web.Scotty.TLS

main :: IO ()
main = scottyTLS 3000 "server.key" "server.crt" $ do
         get "/:word" $ do
             beam <- param "word"
             html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```
