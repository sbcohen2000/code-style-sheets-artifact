{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE GADTs               #-}

module StylishText.Style
  ( applyStyles
  , select
  , selectP ) where

import           Control.Monad
import           Data.Data
import           Data.List            hiding ( union )
import           Data.Map                    ( Map )
import           Data.Maybe
import           Data.Set                    ( Set )
import           StylishText.Types
import           Type.Reflection      as I
import qualified Data.Map             as Map
import qualified Data.Set             as Set

data InProgressSelector = InProgressSelector
  { ipAppliedStyles :: Map Path Styles
  , ipBoundVars     :: [Binding]
  , ipUnapplied     :: [PrimitiveSelectorRule]
  , ipPredicate     :: Predicate
  , ipKeepOutPaths  :: [Path]
  }

-- remove duplicate keys by choosing the key with the highest
-- precedence.
mergeStyles :: Styles -> Styles -> Styles
mergeStyles as =
  (as++)
  |> sortOn (\(k, _v, _prec) -> k)
  |> groupBy (\(k1, _, _) (k2, _, _) -> k1 == k2)
  |> map (maximumBy (\(_, _, prec1) (_, _, prec2) -> compare prec1 prec2))

  where (|>) = flip (.)

-- remove duplicate keys by always choosing a key from
-- a over b when both are present.
mergeStylesBiased :: Styles -> Styles -> Styles
mergeStylesBiased as bs =
  mergeStyles as (map (\(k, v, _) -> (k, v, minBound)) bs)

first :: (a -> c) -> (a, b) -> (c, b)
first f (a, b) = (f a, b)

union :: Map Path Styles -> Map Path Styles -> Map Path Styles
union = Map.unionWith mergeStyles

unions :: [Map Path Styles] -> Map Path Styles
unions = Map.unionsWith mergeStyles

freshInProgressSelector :: Rule -> InProgressSelector
freshInProgressSelector (Rule primitiveSelectors predicate) =
  InProgressSelector { ipAppliedStyles = Map.empty
                     , ipBoundVars = []
                     , ipUnapplied = primitiveSelectors
                     , ipPredicate = predicate
                     , ipKeepOutPaths = [] }

reducePrimitiveSelectorLifetime :: PrimitiveSelector -> PrimitiveSelector
reducePrimitiveSelectorLifetime p@(PatternSelector lifetime ps cls)
  | lifetime == -1 = p -- lifetime of -1 means infinite
  | otherwise = PatternSelector (lifetime - 1) ps cls
reducePrimitiveSelectorLifetime ps@(ClassSelector lifetime cls)
  | lifetime == -1 = ps -- ""
  | otherwise = ClassSelector (lifetime - 1) cls

reducePrimitiveSelectorRuleLifetime
  :: PrimitiveSelectorRule -> PrimitiveSelectorRule
reducePrimitiveSelectorRuleLifetime r =
  r { selector = reducePrimitiveSelectorLifetime (selector r) }

reduceIPSelectorHeadLifetime :: InProgressSelector -> InProgressSelector
reduceIPSelectorHeadLifetime ips = ips { ipUnapplied = newSelectorQueue }
  where
    newSelectorQueue = case ipUnapplied ips of
      [] -> []
      (s:rest) -> reducePrimitiveSelectorRuleLifetime s:rest

selectorHeadIsDead :: InProgressSelector -> Bool
selectorHeadIsDead s =
  case ipUnapplied s of
    [] -> False
    (s:_) -> lifetime (selector s) == 0

reduceHead
  :: Path
  -> InProgressSelector
  -> [Binding]
  -> InProgressSelector
reduceHead p (InProgressSelector applied boundVars unapplied predicate keepOut) binds =
  InProgressSelector { ipAppliedStyles = applied `union` stylesToApply
                     , ipBoundVars = boundVars ++ binds
                     , ipUnapplied = tail unapplied
                     , ipPredicate = predicate
                     , ipKeepOutPaths = keepOutsWithAbsPath ++ keepOut }
  where
    stylesToApply = Map.fromList stylesWithAbsPath
    stylesWithAbsPath = map (first (p++)) styles
    styles = targetedStyles . head $ unapplied

    keepOutsWithAbsPath = map (p++) newKeepOuts
    newKeepOuts = keepOutPaths . head $ unapplied

selectorIsDone :: InProgressSelector -> Bool
selectorIsDone (InProgressSelector _ _ [] _ _) = True
selectorIsDone _                               = False

nSubterms :: Data v => v -> Int
nSubterms = length . gmapQ (const ())

-- NOTE: See profiling remarks about this function
patternDoesMatch
  :: Data v
  => PatternSelector
  -> Path
  -> v
  -> Maybe [Binding]
patternDoesMatch s []     v = s v
patternDoesMatch s (p:ps) v
  | p >= 0 && p < nSubterms v = gmapQi p (patternDoesMatch s ps) v
  | otherwise = error $ "tried to get subterm " ++ show p
                ++ " of a term with just " ++ show (nSubterms v)
                ++ " subterms (" ++ show (dataTypeOf v) ++ ")"

-- test if as is a subset of bs
isSubsetEq :: Eq a => [a] -> [a] -> Bool
isSubsetEq as bs = foldr (\a -> (&& a `elem` bs)) True as

implies :: Bool -> Bool -> Bool
implies True True = True
implies True False = False
implies False _ = True

headDoesApply
  :: Data v
  => v
  -> Path
  -> Classes
  -> InProgressSelector
  -> Maybe [Binding]
headDoesApply v p clss inProgressSelector
  | selectorHeadIsDead inProgressSelector = Nothing
  | shouldKeepOut = Nothing
  | otherwise =
    case selector primitiveSelector of
      PatternSelector _ pat clss'
        | clss' `isSubsetEq` clss -> do
            binds <- patternDoesMatch pat p v
            _ <- guard (isLastPrim `implies` predicateMatches binds)
            pure binds
        | otherwise -> Nothing
      ClassSelector _ clss'
        | clss' `isSubsetEq` clss -> do
            _ <- guard (isLastPrim `implies` predicateMatches [])
            pure []
        | otherwise -> Nothing

  where
    -- If this is the last primitiveSelector in the in-progress
    -- selector, we also need to check that the predicate is true in
    -- order to reduce.
    predicateMatches binds =
      ipPredicate inProgressSelector (ipBoundVars inProgressSelector ++ binds)
    isLastPrim = case ipUnapplied inProgressSelector of
      [_] -> True
      _ -> False

    primitiveSelector = head $ ipUnapplied inProgressSelector
    keepOut = ipKeepOutPaths inProgressSelector
    shouldKeepOut = any (`isPrefixOf` p) keepOut

applySelectorAndReduce
  :: Data v
  => v
  -> Path
  -> Classes
  -> InProgressSelector
  -> InProgressSelector
applySelectorAndReduce v p clss selector =
  case headDoesApply v p clss selector of
    Just binds -> reduceHead p selector binds
    Nothing -> selector

applySelectorAndReduceMaybe
  :: Data v
  => v
  -> Path
  -> Classes
  -> InProgressSelector
  -> Maybe InProgressSelector
applySelectorAndReduceMaybe v p clss selector =
  case headDoesApply v p clss selector of
    Just binds -> Just (reduceHead p selector binds)
    Nothing -> Nothing

-- Return a list of paths between a and b. a and b must have a common
-- prefix.  Exclusive on a, inclusive on b.
--
-- Note to my future self:
--   Before changing this to inclusive on a, consider that in
--   collectStyles, this would cause nodes to be processed twice. The
--   purpose of enumeratePathsBetween is to consider AST nodes that
--   aren't referenced by any node, but should be scrutinized in pattern
--   matching (as all AST nodes should be, regardless of if they have
--   a corresponding StylishText node.
enumeratePathsBetween :: Path -> Path -> [Path]
enumeratePathsBetween (p:ps) (p':ps')
  | p == p' = map (p:) (enumeratePathsBetween ps ps')
  | otherwise = error "paths don't have a common prefix!"
enumeratePathsBetween [] rest = tail $ inits rest
enumeratePathsBetween _ [] = error "paths don't have a common prefix!"

collectStyles
  :: Data v
  => v
  -> Stylesheet
  -> [InProgressSelector]
  -> Maybe Path
  -> StylishText
  -> Map Path Styles
collectStyles _ _     _           _        (TextLeaf _) = Map.empty
collectStyles _ _     _           _        (HtmlLeaf _) = Map.empty
collectStyles v rules activeRules lastPath (Node Nothing _ _ children) =
  -- TODO: It's possible that some rules are reduced here if they only
  -- depend on classes.
  unions $ map (collectStyles v rules activeRules lastPath) children
collectStyles v rules activeRules lastPath (Node (Just path) cls _ children) =
  let paths = maybe [path] (`enumeratePathsBetween` path) lastPath
      f active doneSelectors (p:ps) =
        --  if this is the last path segment, then we have the classes of
        --  the current node (cls). Otherwise, we're operating over a path
        --  which does not have a node, thus it cannot have any classes.
        let classesAtPath = case p:ps of
              [_] -> cls
              _   -> []
            -- first, form the list of rules to reduce at this step:
            --       1) try to apply all in-progress active rules
            --       2) if there are any new rules that apply to this term,
            --          add them into the pool.
            reduced = map (applySelectorAndReduce v p classesAtPath) active
                      ++ mapMaybe (applySelectorAndReduceMaybe v p classesAtPath
                                   . freshInProgressSelector) rules
            -- find the rules that finished at this reduction step
            (newlyDoneSelectors, notDoneSelectors) = partition selectorIsDone reduced
            notDoneSelectors' = map reduceIPSelectorHeadLifetime notDoneSelectors
            styleOrders = unions $ map ipAppliedStyles newlyDoneSelectors
        in f notDoneSelectors' (doneSelectors `union` styleOrders) ps
      f active doneSelectors [] = (active, doneSelectors)
      (activeRules', doneSelectors) = f activeRules Map.empty paths
  in unions $ doneSelectors:map (collectStyles v rules activeRules' (Just path)) children

inheritedProperties :: Set String
inheritedProperties = Set.fromList ["color"]

-- return only the styles with inherited
-- properties
filterInheritedProperties :: Styles -> Styles
filterInheritedProperties =
  filter (\(k, _, _) -> k `Set.member` inheritedProperties)

applyStyles :: Data v => v -> Stylesheet -> StylishText -> StylishText
applyStyles v rules st = go [] st
  where styles = collectStyles v rules [] Nothing st

        go :: Styles -> StylishText -> StylishText
        go _ l@(TextLeaf _) = l
        go _ l@(HtmlLeaf _) = l
        go inherited (Node mp@Nothing cls sty children) =
          Node mp cls sty $ map (go inherited) children
        go inherited (Node mp@(Just path) cls sty children) =
          Node mp cls sty' (map goWithInherited children)
          where sty' = mergeStyles (mergeStyles sty newStyles) inherited
                newStyles = fromMaybe [] (Map.lookup path styles)
                goWithInherited =
                  go (mergeStylesBiased (filterInheritedProperties newStyles) inherited)

-- Selection should succeed if a and b have the same type
-- and applying f to a yields Just, not Nothing
select
  :: forall a b. (Typeable a, Typeable b)
  => (a -> Maybe [Binding])
  -> b
  -> Maybe [Binding]
select f b = case ta `eqTypeRep` tb of
  Just HRefl -> f b
  _ -> Nothing
  where ta = I.typeRep @a
        tb = I.typeRep @b

selectP
  :: forall a b. (Typeable a, Typeable b)
  => (a -> Bool)
  -> b
  -> Bool
selectP f a = case ta `eqTypeRep` tb of
  Just HRefl -> f a
  _ -> False
  where ta = I.typeRep @a
        tb = I.typeRep @b
