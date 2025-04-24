{-# LANGUAGE RankNTypes #-}

module Web.Scotty.TLS
    (  -- * A method for running Scotty over TLS
      scottyTLS
       -- * Transformer version
    , scottyTLSSettings
    , scottyTTLS
    , module Web.Scotty.Trans
    ) where

import           Control.Monad               ((<=<))
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Network.Wai                 (Response)
import           Network.Wai.Handler.Warp    (Port, defaultSettings,
                                              setPort)
import           Network.Wai.Handler.WarpTLS (tlsSettings,
                                              runTLS, TLSSettings(..))
import           Web.Scotty                  (scottyApp, ScottyM, defaultOptions)
import           Web.Scotty.Trans            (ScottyT, scottyAppT)

-- | Run a Scotty application over TLS
scottyTLS :: Port -> FilePath -> FilePath -> ScottyM () -> IO ()
scottyTLS port key cert = runTLS
  (tlsSettings cert key)
  (setPort port defaultSettings) <=< scottyApp

scottyTLSSettings :: Port -> TLSSettings -> ScottyM () -> IO ()
scottyTLSSettings port settings = runTLS
  settings
  (setPort port defaultSettings) <=< scottyApp

scottyTTLS
  :: (Monad m, MonadIO n)
  => Port
  -> FilePath
  -> FilePath
  -> (m Response -> IO Response)
  -> ScottyT m ()
  -> n ()
scottyTTLS port key cert runToIO s = do
 app <- scottyAppT defaultOptions runToIO s
 liftIO $ runTLS
   (tlsSettings cert key)
   (setPort port defaultSettings)
   app
