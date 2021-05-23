{-# LANGUAGE TemplateHaskell #-}

-- | Effect for handling metrics
module Calamity.Metrics.Eff (
  Counter,
  Gauge,
  Histogram,
  HistogramSample (..),
  MetricEff (..),
  registerCounter,
  registerGauge,
  registerHistogram,
  addCounter,
  modifyGauge,
  observeHistogram,
) where

import Calamity.Internal.Utils (DefaultingMap)
import Calamity.Metrics.Internal
import Data.Default.Class
import Data.Map
import Data.Text
import GHC.Generics
import Polysemy
import TextShow
import Calamity.Internal.OverriddenVia

data HistogramSample' = HistogramSample'
  { buckets :: DefaultingMap Double Double
  , sum :: Double
  , count :: Int
  }
  deriving (Generic)
  deriving (Default)

data HistogramSample = HistogramSample
  { buckets :: Map Double Double
  , sum :: Double
  , count :: Int
  }
  deriving (Eq, Show, Generic)
  deriving (Default) via OverriddenVia HistogramSample HistogramSample
  deriving
    (TextShow)
    via FromStringShow HistogramSample

data MetricEff m a where
  -- | Register a 'Counter'
  RegisterCounter ::
    -- | Name
    Text ->
    -- | Labels
    [(Text, Text)] ->
    MetricEff m Counter
  -- | Register a 'Gauge'
  RegisterGauge ::
    -- | Name
    Text ->
    -- | Labels
    [(Text, Text)] ->
    MetricEff m Gauge
  -- | Register a 'Histogram'
  RegisterHistogram ::
    -- | Name
    Text ->
    -- | Labels
    [(Text, Text)] ->
    -- | Upper bounds
    [Double] ->
    MetricEff m Histogram
  AddCounter :: Int -> Counter -> MetricEff m Int
  ModifyGauge :: (Double -> Double) -> Gauge -> MetricEff m Double
  ObserveHistogram :: Double -> Histogram -> MetricEff m HistogramSample

makeSem ''MetricEff
