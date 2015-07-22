{-# LANGUAGE RankNTypes #-}

module Web.Scotty.TLS
    (  -- * A method for running Scotty over TLS
      scottyTLS
       -- * Transformer version
    , scottyTTLS
    , module Web.Scotty.Trans
    ) where

import           Control.Monad               ((<=<))
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Network.Wai                 (Response)
import           Network.Wai.Handler.Warp    (Port, defaultSettings,
                                              setPort)
import           Network.Wai.Handler.WarpTLS (certFile, defaultTlsSettings,
                                              keyFile, runTLS)
import           Web.Scotty                  (scottyApp, ScottyM)
import           Web.Scotty.Trans            (ScottyT, scottyAppT)

-- | Run a Scotty application over TLS
scottyTLS :: Port -> FilePath -> FilePath -> ScottyM () -> IO ()
scottyTLS port key cert = runTLS
  (defaultTlsSettings { keyFile = key , certFile = cert })
  (setPort port defaultSettings) <=< scottyApp

scottyTTLS :: (Monad m, MonadIO n) => Port -> FilePath -> FilePath ->
              (m Response -> IO Response) -> ScottyT t m () -> n ()
scottyTTLS port key cert runToIO s = scottyAppT runToIO s >>= liftIO . runTLS
                                              (defaultTlsSettings { keyFile = key, certFile = cert })
                                              (setPort port defaultSettings)

