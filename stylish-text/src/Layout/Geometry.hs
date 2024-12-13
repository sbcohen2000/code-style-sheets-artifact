{-# LANGUAGE OverloadedStrings #-}

module Layout.Geometry where

import           Control.Applicative      (empty)
import           Data.Aeson               ((.=), (.:))
import qualified Data.Aeson          as J

data Rect a = Rect
  { upperLeft  :: Point a
  , lowerRight :: Point a
  }
  deriving (Show, Eq)

instance (J.ToJSON a, Num a) => J.ToJSON (Rect a) where
  toJSON r = J.object
    [ "x"     .= left  r, "y"      .= top    r
    , "top"   .= top   r, "left"   .= left   r
    , "width" .= width r, "height" .= height r
    , "right" .= right r, "bottom" .= bottom r ]

instance (Ord a, J.FromJSON a) => J.FromJSON (Rect a) where
  parseJSON (J.Object v) = fromLeftRightTopBottom
    <$> v .: "left"
    <*> v .: "right"
    <*> v .: "top"
    <*> v .: "bottom"
  parseJSON _ = empty

data Point a = Point
  { x::a
  , y::a
  }
  deriving (Show, Eq)

data Size a = Size
  { w::a
  , h::a
  }
  deriving (Show, Eq)

instance (J.ToJSON a, Num a) => J.ToJSON (Size a) where
  toJSON s = J.object [ "width"  .= width s
                      , "height" .= height s ]

class Sized s where
  width  :: Num a => s a -> a
  height :: Num a => s a -> a

instance Sized Rect where
  width r = right r - left r
  height r = bottom r - top r

instance Sized Size where
  width = w
  height = h

normalizeRect :: Ord a => Rect a -> Rect a
normalizeRect r =
  Rect { upperLeft  = Point minX minY
       , lowerRight = Point maxX maxY }
  where minX = min (left r) (right r)
        maxX = max (left r) (right r)
        minY = min (top r) (bottom r)
        maxY = max (top r) (bottom r)

left :: Rect a -> a
left = x . upperLeft

right :: Rect a -> a
right = x . lowerRight

top :: Rect a -> a
top = y . upperLeft

bottom :: Rect a -> a
bottom = y . lowerRight

fromUpperLeftAndLowerRight :: Ord a => Point a -> Point a -> Rect a
fromUpperLeftAndLowerRight a = normalizeRect . Rect a

fromLeftRightTopBottom :: Ord a => a -> a -> a -> a -> Rect a
fromLeftRightTopBottom left right top bottom =
  fromUpperLeftAndLowerRight upperLeft lowerRight
  where upperLeft = Point left top
        lowerRight = Point right bottom

intersection :: Ord a => Rect a -> Rect a -> Rect a
intersection ra rb =
  fromLeftRightTopBottom l r t b
  where l = max (left   ra) (left   rb)
        r = min (right  ra) (right  rb)
        t = max (top    ra) (top    rb)
        b = min (bottom ra) (bottom rb)

size :: (Sized s, Num a) => s a -> Size a
size s = Size (width s) (height s)
