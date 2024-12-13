{-# LANGUAGE ImpredicativeTypes #-}

module StylishText.Types where

import Data.Data

type Styles     = [(StyleProp, StyleVal, Precedence)]
type StyleProp  = String
type StyleVal   = String
type Precedence = Int
type Classes    = [ClassName]
type ClassName  = String
type Path       = [Int] -- 0-indexed

data StylishText
  = TextLeaf String
  | HtmlLeaf String
  | Node (Maybe Path) Classes Styles [StylishText]

stylishTextToDebugString :: StylishText -> String
stylishTextToDebugString = impl 0
  where
    impl indent (TextLeaf s) = indentStr indent ++ "<<text leaf>> \"" ++ s ++ "\"\n"
    impl indent (HtmlLeaf s) = indentStr indent ++ "<<html leaf>> \"" ++ s ++ "\"\n"
    impl indent (Node mp cls sty children) =
      indentStr indent ++ "<<node>>" ++ "\n"
      ++ indentStr indent ++ "   path: " ++ show mp  ++ "\n"
      ++ indentStr indent ++ "classes: " ++ show cls ++ "\n"
      ++ indentStr indent ++ " styles: " ++ show sty ++ "\n"
      ++ concatMap (impl (indent + 1)) children

    indentStr n = replicate (4 * n) ' '

class Stylish t where
  showStylish :: Path -> t -> StylishText

type PatternSelector = forall a. Data a => a -> Maybe [Binding]

data PrimitiveSelector
  = PatternSelector Int PatternSelector Classes
  | ClassSelector   Int Classes
  --                ^ lifetime

lifetime :: PrimitiveSelector -> Int
lifetime (PatternSelector l _ _) = l
lifetime (ClassSelector l _) = l

data Binding = forall a. (Typeable a, Data a) => Binding a

type Predicate = [Binding] -> Bool

type TargetedStyles = [(Path, Styles)]

data PrimitiveSelectorRule =
  PrimitiveSelectorRule
  { selector       :: PrimitiveSelector
  , targetedStyles :: TargetedStyles
  , keepOutPaths   :: [Path]
  }

-- TODO:
-- data ComplexSelector
--   = SelectPrimitive PrimitiveSelector
--   | SelectDescendant ComplexSelector ComplexSelector
--
-- data Selector = Selector [ComplexSelector] Predicate

data Rule = Rule [PrimitiveSelectorRule] Predicate

type Stylesheet = [Rule]
