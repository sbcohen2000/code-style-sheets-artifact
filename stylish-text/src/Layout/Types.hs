{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Layout.Types
  ( ComputedStyle(..)
  , DrawOrder(..)
  , Exclusive(..)
  , Flip(..)
  , Fragment(..)
  , Inclusive(..)
  , InspectorInfo(..)
  , LayoutMetrics(..)
  , LineMetric(..)
  , LineNumber(..)
  , MeasurementSpec(..)
  , Renderable(..)
  , Side(..)
  , Span(..)
  , Styles
  , WithId(..)
  , begin
  , end
  ) where

import Data.Aeson ( (.=) )
import Data.IntMap ( IntMap )
import Data.Vector ( Vector )

import Layout.Geometry ( Size )
import qualified Data.Aeson as J

type Styles = [(String, String)]

data InspectorInfo
  = InspectorInfo
  { iiPath :: Maybe [Int]
  , iiCls  :: [String]
  }
  deriving Show

instance J.ToJSON InspectorInfo where
  toJSON (InspectorInfo path classes) =
    J.object [ "path"    .= path
             , "classes" .= classes ]

data ComputedStyle
  = ComputedStyle
    { csPadding         :: Double
    , csMargin          :: Double
    , csBorderWidth     :: Double
    , csBorderColor     :: String
    , csBackgroundColor :: String
    , csBorderRadius    :: Double
    , csOtherStyles     :: Styles
    , csInspectorInfo   :: InspectorInfo
    }
  deriving Show

instance J.ToJSON ComputedStyle where
  toJSON (ComputedStyle {..}) =
    J.object [ "padding"         .= csPadding
             , "margin"          .= csMargin
             , "borderWidth"     .= csBorderWidth
             , "borderColor"     .= csBorderColor
             , "backgroundColor" .= csBackgroundColor
             , "borderRadius"    .= csBorderRadius
             , "otherStyles"     .= csOtherStyles
             , "inspectorInfo"   .= csInspectorInfo ]

data MeasurementSpec
  = MeasureText String Styles
  | MeasureHtml String Styles

instance J.ToJSON MeasurementSpec where
  toJSON (MeasureText text styles) =
    J.object [ "op" .= ("MeasureText"::String)
             , "text" .= text
             , "styles" .= styles ]
  toJSON (MeasureHtml contents styles) =
    J.object [ "op" .= ("MeasureHtml"::String)
             , "contents" .= contents
             , "styles" .= styles ]

class Renderable r m where
  style       :: r -> m ComputedStyle
  children    :: r -> m [Either Fragment r]

data WithId a = WithId Int a

data Fragment
  = FNewline
  | FText String (Size Double)
  | FHtml String (Size Double)
  deriving (Eq, Show)

instance J.ToJSON (WithId Fragment) where
  toJSON (WithId id FNewline) =
    J.object [ "type" .= ("FNewline"::String)
             , "id"   .= id ]
  toJSON (WithId id (FText text size)) =
    J.object [ "type" .= ("FText"::String)
             , "id"   .= id
             , "text" .= text
             , "size" .= size ]
  toJSON (WithId id (FHtml contents size)) =
    J.object [ "type"     .= ("FHtml"::String)
             , "id"       .= id
             , "contents" .= contents
             , "size"     .= size ]

data LineMetric =
  LineMetric
  { top       ::Double
  , aboveSpace::Double
  , lineHeight::Double
  , belowSpace::Double
    -- bottom = top + aboveSpace + lineHeight + belowSpace
  }
  deriving Show

instance J.ToJSON LineMetric where
  toJSON LineMetric{..} =
    J.object [ "top"        .= top
             , "aboveSpace" .= aboveSpace
             , "lineHeight" .= lineHeight
             , "belowSpace" .= belowSpace ]

newtype LineNumber = LineNumber { getLineNumber::Int }
  deriving (Eq, Ord, Show)

instance Enum LineNumber where
  toEnum = LineNumber
  fromEnum = getLineNumber

instance J.ToJSON LineNumber where
  toJSON (LineNumber n) = J.toJSON n

data Side = Above | Below
  deriving (Show, Eq, Ord)

instance J.ToJSON Side where
  toJSON Above = "above"
  toJSON Below = "below"

data Flip a = Flip a | DontFlip a
  deriving (Show, Eq)

instance Functor Flip where
  fmap f (Flip     a) = Flip     $ f a
  fmap f (DontFlip a) = DontFlip $ f a

instance Ord a => Ord (Flip a) where
  compare (Flip     a) (Flip     b) = compare a b
  compare (DontFlip a) (DontFlip b) = compare a b
  compare (Flip     a) (DontFlip b) = compare a b
  compare (DontFlip a) (Flip     b) = compare a b

newtype Inclusive = Inclusive ()
newtype Exclusive = Exclusive ()

newtype Span a b = Span (b, b)

begin :: Span a b -> b
begin (Span (b, _)) = b

end :: Span a b -> b
end (Span (_, b)) = b

instance J.ToJSON (Flip (Span Exclusive Double)) where
  toJSON (Flip a)=
    J.object [ "begin" .= begin a
             , "end"   .= end a
             , "flip"  .= True ]
  toJSON (DontFlip a) =
    J.object [ "begin" .= begin a
             , "end"   .= end a
             , "flip"  .= False ]

instance J.ToJSON a => J.ToJSON (Span Inclusive a) where
  toJSON a = J.object [ "begin" .= begin a
                      , "end"   .= end a ]

instance J.ToJSON a => J.ToJSON (Span Exclusive a) where
  toJSON a = J.object [ "begin" .= begin a
                      , "end"   .= end a ]

data DrawOrder
  = HorzLine
    {
      doLine   :: LineNumber
    , doSide   :: Side
    , doOffset :: Double
    , doSpan   :: Flip (Span Exclusive Double)
    }
  | ClosePath

instance J.ToJSON DrawOrder where
  toJSON HorzLine{..} =
    J.object [ "type"   .= ("HorzLine"::String)
             , "line"   .= doLine
             , "side"   .= doSide
             , "offset" .= doOffset
             , "span"   .= doSpan ]
  toJSON ClosePath =
    J.object [ "type" .= ("ClosePath"::String) ]

data LayoutMetrics
  = LayoutMetrics
  { interLineSpacing ::Vector LineMetric
  , drawOrder        ::IntMap [DrawOrder]
  }

instance J.ToJSON LayoutMetrics where
  toJSON LayoutMetrics{..} =
    J.object [ "interLineSpacing" .= interLineSpacing
             , "drawOrder"        .= drawOrder ]

