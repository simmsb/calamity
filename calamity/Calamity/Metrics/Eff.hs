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

import Calamity.Metrics.Internal
import Data.Default.Class
import qualified Data.Map as Map
import Data.Text
import Optics.TH
import Polysemy
import TextShow

data HistogramSample = HistogramSample
  { buckets :: Map.Map Double Double
  , sum :: Double
  , count :: Int
  }
  deriving (Eq, Show)
  deriving
    (TextShow)
    via FromStringShow HistogramSample

instance Default HistogramSample where
  def = HistogramSample Map.empty 0.0 0

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

$(makeFieldLabelsNoPrefix ''HistogramSample)
