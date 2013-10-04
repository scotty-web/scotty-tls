{-# LANGUAGE RankNTypes #-}

module Web.Scotty.TLS
    ( scottyTLS -- * A method for running Scotty over TLS
    , scottyTTLS -- * Transformer version
    , module Web.Scotty.Trans -- * Re-export the Trans module
    ) where

import           Control.Monad               ((<=<))
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Network.Wai                 (Response)
import           Network.Wai.Handler.Warp    (Port, defaultSettings,
                                              settingsPort)
import           Network.Wai.Handler.WarpTLS (certFile, defaultTlsSettings,
                                              keyFile, runTLS)
import           Web.Scotty                  (scottyApp)
import           Web.Scotty.Trans            (ScottyM, ScottyT, scottyAppT)

-- | Run a Scotty application over TLS
scottyTLS :: Port -> FilePath -> FilePath -> ScottyM () -> IO ()
scottyTLS port key cert = runTLS
  (defaultTlsSettings { keyFile = key , certFile = cert })
  (defaultSettings { settingsPort = port }) <=< scottyApp

scottyTTLS :: (Monad m, MonadIO n) => Port -> FilePath -> FilePath -> (forall a. m a -> n a) -> (m Response -> IO Response) -> ScottyT m () -> n ()
scottyTTLS port key cert runM runToIO s = scottyAppT runM runToIO s >>= liftIO . runTLS (defaultTlsSettings { keyFile = key, certFile = cert })
                                                                                        (defaultSettings { settingsPort = port })

