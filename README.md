# Scotty-TLS

You can test by generating a self-signed certificate [here](http://www.akadia.com/services/ssh_test_certificate.html).

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