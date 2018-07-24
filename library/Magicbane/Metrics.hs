{-# LANGUAGE UnicodeSyntax, FlexibleInstances, UndecidableInstances #-}

-- | Provides metrics via monad-metrics/EKG in a Magicbane app context.
--   Also reexports Wai metrics middleware.
module Magicbane.Metrics (
  module Magicbane.Metrics
, module X
) where

import qualified Control.Monad.Metrics
import           Control.Monad.Metrics as X hiding (initialize, initializeWith, run, run')
import           System.Metrics as X (Store, registerGcMetrics)
import qualified System.Remote.Monitoring.Wai
import           System.Remote.Monitoring.Wai as X (serverMetricStore)
import           Network.Wai.Metrics as X
import           Data.ByteString (ByteString)

newtype ModMetrics = ModMetrics Metrics

forkMetricsServer ∷ ByteString → Int → IO System.Remote.Monitoring.Wai.Server
forkMetricsServer = System.Remote.Monitoring.Wai.forkServer

-- | Creates a metrics module with a particular Store.
--   The Store should come from the backend you want to use for storing the metrics.
--   For development, a simple backend that shows metrics on a web page is ekg-wai, reexported here.
newMetricsWith ∷ Store → IO ModMetrics
newMetricsWith x = ModMetrics <$> Control.Monad.Metrics.initializeWith x
