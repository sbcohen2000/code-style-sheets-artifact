module StylishText
  ( applyStyles
  , select
  , selectP
  , Path -- TODO: this should not be exported
  , ClassName, Classes
  , Precedence
  , Styles
  , StylishText(..)
  , stylishTextToDebugString
  , Stylish
  , showStylish
  , PatternSelector
  , TargetedStyles
  , PrimitiveSelectorRule(..)
  , Binding(..)
  , Predicate
  , PrimitiveSelector(..)
  , Rule(..)
  , Stylesheet
  ) where

import StylishText.Style (applyStyles, select, selectP)
import StylishText.Types
