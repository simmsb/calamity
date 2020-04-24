{-# LANGUAGE TemplateHaskell #-}

-- | Effect for handling metrics
module Calamity.Metrics.Eff
    ( Counter
    , Gauge
    , Histogram
    , HistogramSample
    , MetricEff(..)
    , registerCounter
    , registerGauge
    , registerHistogram
    , addCounter
    , modifyGauge
    , observeHistogram ) where

import           Calamity.Internal.Utils   ()
import           Calamity.Metrics.Internal

import           Data.Default.Class
import           Data.Map
import           Data.Text

import           GHC.Generics

import           Polysemy

import           TextShow
import qualified TextShow.Generic          as TSG

data HistogramSample = HistogramSample
  { buckets :: Map Double Double
  , sum     :: Double
  , count   :: Int
  }
  deriving ( Eq, Show, Generic, Default )
  deriving ( TextShow ) via TSG.FromGeneric HistogramSample

data MetricEff m a where
  -- | Register a 'Counter'
  RegisterCounter :: Text -- ^ Name
    -> [(Text, Text)] -- ^ Labels
    -> MetricEff m Counter

  -- | Register a 'Gauge'
  RegisterGauge :: Text -- ^ Name
    -> [(Text, Text)] -- ^ Labels
    -> MetricEff m Gauge

  -- | Register a 'Histogram'
  RegisterHistogram :: Text -- ^ Name
    -> [(Text, Text)] -- ^ Labels
    -> [Double] -- ^ Upper bounds
    -> MetricEff m Histogram

  AddCounter :: Int -> Counter -> MetricEff m Int

  ModifyGauge :: (Double -> Double) -> Gauge -> MetricEff m Double

  ObserveHistogram :: Double -> Histogram -> MetricEff m HistogramSample

makeSem ''MetricEff
