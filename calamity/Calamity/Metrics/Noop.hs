-- | Noop handler for metrics
module Calamity.Metrics.Noop
    ( runMetricsNoop ) where

import           Calamity.Metrics.Eff
import           Calamity.Metrics.Internal

import           Data.Default.Class

import           Polysemy

runMetricsNoop :: Sem (MetricEff ': r) a -> Sem r a
runMetricsNoop = interpret $ \case
  RegisterCounter _ _     -> pure (Counter 0)
  RegisterGauge _ _       -> pure (Gauge 0)
  RegisterHistogram _ _ _ -> pure (Histogram 0)

  AddCounter _ _          -> pure def
  ModifyGauge _ _         -> pure def
  ObserveHistogram _ _    -> pure def
