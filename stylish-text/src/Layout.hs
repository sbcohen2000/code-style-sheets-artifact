{-# LANGUAGE OverloadedStrings #-}

module Layout
  ( ComputedStyle(..)
  , Fragment(..)
  , InspectorInfo(..)
  , MeasurementSpec(..)
  , Renderable(..)
  , Styles
  , layout ) where

import Control.Monad.IO.Class
import Data.Aeson

import Layout.Types
import qualified Layout.Algorithm ( layout )

layout
  :: (MonadIO m, Renderable r m)
  => Double
  -> r
  -> m Value
layout leading renderable = do
  (metrics, fragments, root) <- Layout.Algorithm.layout leading renderable
  pure $ object [ "metrics"   .= metrics
                , "fragments" .= fragments
                , "root"      .= root ]
