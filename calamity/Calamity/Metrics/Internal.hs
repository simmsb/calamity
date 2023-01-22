-- | Internal data structures to the metrics effect
module Calamity.Metrics.Internal (
  Counter (..),
  Gauge (..),
  Histogram (..),
) where

-- | A handle to a counter
newtype Counter = Counter
  { unCounter :: Int
  }

-- | A handle to a gauge
newtype Gauge = Gauge
  { unGauge :: Int
  }

-- | A handle to a histogram
newtype Histogram = Histogram
  { unHistogram :: Int
  }
