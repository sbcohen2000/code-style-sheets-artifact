{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedStrings  #-}

module Layout.Algorithm ( layout ) where

import Control.Lens hiding ( (|>), (.=) )
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson ( (.=) )
import Data.Char (isSpace)
import Data.IntMap ( IntMap )
import Data.IntSet ( IntSet )
import Data.Map ( Map )
import Data.Maybe ( catMaybes )
import Data.Vector ( Vector )
import Data.Vector.Lens ( sliced, ordinals )
import qualified Data.Aeson as J
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Vector as V

import Layout.IntervalTree ( IntervalTree )
import Layout.Types
import qualified Layout.Geometry as G
import qualified Layout.IntervalTree as IT

data FTTerm = FTTerm
  { children     ::[FTTerm]
  , id           ::Int
  , computedStyle::ComputedStyle
  , fragmentSpan ::Span Exclusive Int
  , lineSpan     ::Span Inclusive LineNumber
  }

instance J.ToJSON FTTerm where
  toJSON (FTTerm children id style fragments lines) =
    J.object [ "children"  .= children
             , "style"     .= style
             , "id"        .= id
             , "lines"     .= lines
             , "fragments" .= fragments ]

data Gadget =
    GShim           Int Double -- width
  | GAdjustableShim Int
  deriving Show

instance J.ToJSON Gadget where
  toJSON (GShim id width)     =
    J.object [ "type"  .= ("GShim"::String)
             , "id"    .= id
             , "width" .= width ]
  toJSON (GAdjustableShim id) =
    J.object [ "type"  .= ("GAdjustableShim"::String)
             , "id"    .= id ]

type Fragments = Vector ([Gadget], WithId Fragment, [Gadget])

data FragmentTree = FragmentTree
  { root          ::FTTerm
  , idToTerm      ::IntMap FTTerm
  , fragments     ::Fragments
  , lineMap       ::IntMap (Span Exclusive Int)
  , interLineSpans::Map (LineNumber, Side) (IntervalTree Double Double)
  -- hierarchyLUT is used to efficiently ask the question
  -- "is term A an descendant of term B?" The keys of the map
  -- are element ids and the values are sets of ancestor ids.
  , hierarchyLUT  ::IntMap IntSet
  , leading       ::Double
  }

class SpanLike a where
  empty :: a -> Bool

instance Ord a => SpanLike (Span Inclusive a) where
  empty (Span (a, b)) = a > b

instance Ord a => SpanLike (Span Exclusive a) where
  empty (Span (a, b)) = a >= b

fragmentId :: WithId Fragment -> Int
fragmentId (WithId id _) = id

gadgetId :: Gadget -> Int
gadgetId (GShim           id _) = id
gadgetId (GAdjustableShim id  ) = id

spanIntersection :: (Ord b) => Span a b -> Span a b -> Span a b
spanIntersection (Span (a1, a2)) (Span (b1, b2)) = Span (max a1 b1, min a2 b2)

spanExpandEnd :: (Num b) => b -> Span a b -> Span a b
spanExpandEnd amount (Span (b, e)) = Span (b, e + amount)

requiredMargin :: ComputedStyle -> Double
requiredMargin ComputedStyle{..} =
  -- note: border is drawn on centerline, so we need to account for
  -- half of its width
  csMargin + csPadding + csBorderWidth

-- True if a is an ancestor of b.
isAncestorOf :: IntMap IntSet -> Int -> Int -> Bool
isAncestorOf map a b =
  IntSet.member b $ IntMap.findWithDefault IntSet.empty a map

data ToFragmentTreeState
  = ToFragmentTreeState
  { tfsFreshId       :: Int
  , tfsIndex         :: Int
  , tfsLine          :: LineNumber
  , tfsFragments     :: Fragments
    -- tfsLineMap maps from integer line numbers to [begin, end)
    -- indices. These indices map to the range of fragments on that
    -- line.
  , tfsLineMap       :: IntMap (Span Exclusive Int)
  , tfsHierarchyLUT  :: IntMap IntSet
  , tfsIdToTerm      :: IntMap FTTerm
  }

-- get a fresh id
tfsGetFreshId :: (Monad m) => StateT ToFragmentTreeState m Int
tfsGetFreshId =
  modify (\s -> s { tfsFreshId = tfsFreshId s + 1 })
  >> gets tfsFreshId

forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM mbs f = catMaybes <$> mapM f mbs

-- update the ToFragmentTreeState to reflect that the fragment with
-- index `index` is on line `lineNumber`.
fragmentIsOnLine
  :: Monad m
  => Int
  -> LineNumber
  -> StateT ToFragmentTreeState m ()
fragmentIsOnLine index lineNumber = do
  record <- gets (IntMap.lookup (getLineNumber lineNumber) . tfsLineMap)
  let record' = case record of
        Nothing -> Span (index, index + 1)
        Just (Span (begin, end)) -> Span (min begin index, max end (index + 1))
  modify (\s -> s { tfsLineMap =
                    IntMap.insert (getLineNumber lineNumber) record' (tfsLineMap s) })

renderableToFragmentTreeImpl
  :: (MonadIO m, Renderable r m)
  => IntSet
  -> r
  -> StateT ToFragmentTreeState m FTTerm
renderableToFragmentTreeImpl parentIds renderable = do
  id <- tfsGetFreshId
  renderableChildren <- lift $ Layout.Types.children renderable
  style <- lift $ Layout.Types.style renderable

  begin <- gets tfsIndex
  beginLine <- gets tfsLine

  childTerms <- forMaybeM renderableChildren $ \case
    Left fragment -> do
      currentIndex <- gets tfsIndex
      currentLine  <- gets tfsLine
      let withGadgets = ([], WithId id fragment, [])
      modify (\s -> s { tfsIndex = tfsIndex s + 1
                      , tfsFragments = V.snoc (tfsFragments s) withGadgets })
      fragmentIsOnLine currentIndex currentLine
      when (fragment == FNewline) $
        modify (\s -> s { tfsLine = succ $ tfsLine s })
      pure Nothing

    Right r ->
      Just <$> renderableToFragmentTreeImpl (IntSet.insert id parentIds) r

  end       <- gets tfsIndex
  endLine   <- gets tfsLine

  -- update tfsHierarchyLUT: "my (id) ancestors are parentIds"
  modify (\s -> s { tfsHierarchyLUT = IntMap.insertWith IntSet.union id parentIds (tfsHierarchyLUT s) })

  let theTerm = FTTerm
        { children = childTerms
        , id
        , computedStyle = style
        , fragmentSpan = Span (begin, end)
        , lineSpan = Span (beginLine, endLine) }
  modify (\s -> s { tfsIdToTerm = IntMap.insert id theTerm (tfsIdToTerm s) })
  pure theTerm

renderableToFragmentTree
  :: (MonadIO m, Renderable r m)
  => Double
  -> r
  -> m FragmentTree
renderableToFragmentTree leading renderable =
  let initialState = ToFragmentTreeState
        { tfsFreshId        = 0
        , tfsIndex          = 0
        , tfsLine           = LineNumber 0
        , tfsFragments      = V.empty
        , tfsLineMap        = IntMap.empty
        , tfsHierarchyLUT   = IntMap.empty
        , tfsIdToTerm       = IntMap.empty }
  in do
    (root, finalState) <- runStateT
      (renderableToFragmentTreeImpl IntSet.empty renderable)
      initialState
    pure $ FragmentTree
      { root
      , fragments      = tfsFragments    finalState
      , idToTerm       = tfsIdToTerm     finalState
      , lineMap        = tfsLineMap      finalState
      , interLineSpans = Map.empty
      , hierarchyLUT   = tfsHierarchyLUT finalState
      , leading
      }

newtype ComputeWidthEnv = ComputeWidthEnv
  { wcsLineMap :: IntMap (Span Exclusive Int)
  }

newtype ComputeWidthState = ComputeWidthState
  { wcsFragments::Fragments
  }

type ComputeWidth a = ReaderT ComputeWidthEnv (State ComputeWidthState) a

modifyFragments
  :: (Fragments -> Fragments)
  -> ComputeWidth ()
modifyFragments f = modify (\s -> s { wcsFragments = f $ wcsFragments s })

type PendingShim = Maybe Gadget

addShimsAtNewlinesImpl
  :: Int
  -> Double
  -> Fragments
  -> State PendingShim Fragments
addShimsAtNewlinesImpl parentId width =
  V.mapM $ \elem -> do
  case elem of
     -- put shims after newlines
    (gadgetsBefore, WithId id FNewline, gadgetsAfter) -> do
      unless (width == 0) $ put $ Just (GShim parentId width)
      pure (gadgetsBefore
           , WithId id FNewline
           , if width == 0
             then GAdjustableShim parentId:gadgetsAfter
             else GShim parentId width:GAdjustableShim parentId:gadgetsAfter)
    (_, fragment, _) ->
      if isFragmentWhitespace fragment then
        -- wait until we see a non-whitespace fragment
        pure elem
      else do
        -- place the shim
        maybePendingShim <- get
        put Nothing
        pure $ case maybePendingShim of
          Just shim -> over _1 (shim:) elem
          _ -> elem

addShimsAtNewlines :: Int -> Double -> Fragments -> Fragments
addShimsAtNewlines parentId width frags =
  evalState (addShimsAtNewlinesImpl parentId width frags) Nothing

isNewline :: (a, WithId Fragment, b) -> Bool
isNewline (_, WithId _ FNewline, _) = True
isNewline _                         = False

computeWidthImpl :: FTTerm -> ComputeWidth ()
computeWidthImpl FTTerm {..} = do
  mapM_ computeWidthImpl children
  let b = begin fragmentSpan
      e = end   fragmentSpan
  unless (b == e) $ do
    -- Add a shim before the first character of the first fragment.
    modifyFragments $ over (ordinals [b] . _1)
      (consShimIfNecessary id computedStyle)
    -- Add a shim after the last character of the last fragment unless
    -- the last fragment is a newline. (Then, the shim will be added
    -- in the 3rd step).
    lastIsNewline <- gets (isNewline . (V.! pred e) . wcsFragments)
    unless lastIsNewline $
      modifyFragments $ over (ordinals [pred e] . _3)
      (consShimIfNecessary id computedStyle)
    -- For every line which contains a newline, add a shim on the first
    -- character of the line, and a variable shim after the last character.
    modifyFragments $ over (sliced b (e - b))
      (addShimsAtNewlines id (requiredMargin computedStyle))

      -- cons a shim onto a list, but only if it's width is non-zero
      where consShimIfNecessary :: Int -> ComputedStyle -> [Gadget] -> [Gadget]
            consShimIfNecessary id computedStyle =
              let width = requiredMargin computedStyle
              in if width == 0
                 then Prelude.id
                 else (GShim id width:)

computeWidth :: FragmentTree -> FragmentTree
computeWidth tree =
  let initialEnv   = ComputeWidthEnv $ lineMap tree
      initialState = ComputeWidthState $ fragments tree
      (_, finalState) = flip runState initialState $
                        flip runReaderT initialEnv $ do
        computeWidthImpl (root tree)
        resolveAdjustableShims (root tree)
  in tree { fragments = wcsFragments finalState }

fragmentWidth :: WithId Fragment -> Double
fragmentWidth (WithId _ FNewline      ) = 0.0
fragmentWidth (WithId _ (FText _ size)) = G.width size
fragmentWidth (WithId _ (FHtml _ size)) = G.width size

fragmentHeight :: WithId Fragment -> Double
fragmentHeight (WithId _ FNewline      ) = 0.0
fragmentHeight (WithId _ (FText _ size)) = G.height size
fragmentHeight (WithId _ (FHtml _ size)) = G.height size

isFragmentWhitespace :: WithId Fragment -> Bool
isFragmentWhitespace (WithId _ FNewline      ) = True
isFragmentWhitespace (WithId _ (FText text _)) = all isSpace text
isFragmentWhitespace (WithId _ (FHtml html _)) = all isSpace html

gadgetWidth :: Gadget -> Double
gadgetWidth (GShim           _ width) = width
gadgetWidth (GAdjustableShim _      ) = 0.0

forAllObjectsInRangeM_
  :: (Monad m)
  => Fragments
  -> Span Exclusive Int
  -> (Int -> Double -> WithId Fragment -> m a)
  -> m ()
forAllObjectsInRangeM_ fragments (Span (a, b)) f
  | a >= b = pure ()
  | otherwise = do
      let (gadgetsBefore, frag, gadgetsAfter) = fragments V.! a
      mapM_ (mapper frag) gadgetsBefore
      _ <- f (fragmentId frag) (fragmentWidth frag) frag
      mapM_ (mapper frag) (reverse gadgetsAfter)
      forAllObjectsInRangeM_ fragments (Span (a + 1, b)) f
        where mapper parentFrag g = f (gadgetId g) (gadgetWidth g) parentFrag

lineWidth :: LineNumber -> ComputeWidth Double
lineWidth lineNumber = do
  fragments <- gets wcsFragments
  lineSpan <- asks (IntMap.findWithDefault (Span (0,0))
                    (getLineNumber lineNumber) . wcsLineMap)
  pure $ flip evalState 0 $
    forAllObjectsInRangeM_ fragments lineSpan (\_ width _ -> (modify . (+)) width) >> get

updateFirstAdjustableShim :: Double -> Fragments -> Fragments
updateFirstAdjustableShim width =
  snd . V.foldl
  (\(done, v) elem@(gadgetsBefore, frag, gadgetsAfter) ->
     if done then (True, V.snoc v elem)
     else
       let (gadgetsBefore', didUpdateBefore) = updGadgets gadgetsBefore
           (gadgetsAfter',  didUpdateAfter ) = updGadgets (reverse gadgetsAfter)
       in if | didUpdateBefore ->
               (True,  V.snoc v (gadgetsBefore', frag, gadgetsAfter))
             | didUpdateAfter  ->
               (True,  V.snoc v (gadgetsBefore,  frag, reverse gadgetsAfter'))
             | otherwise       ->
               (False, V.snoc v elem))
  (False, V.empty)
  where updGadgets [] = ([], False)
        updGadgets (GAdjustableShim id:rest)
          | width == 0 = (rest, True)
          | otherwise = (GShim id width:rest, True)
        updGadgets (shim:rest) =
          let (rest', didUpdate) = updGadgets rest
          in (shim:rest', didUpdate)

updateFirstAdjustableShimInLine
  :: Span Exclusive Int
  -> LineNumber
  -> Double
  -> ComputeWidth ()
updateFirstAdjustableShimInLine range lineNumber width = do
  lineSpan <- asks (IntMap.findWithDefault (Span (0, 0))
                     (getLineNumber lineNumber) . wcsLineMap)
  let Span (b, e) = spanIntersection range lineSpan
  modifyFragments $
    over (sliced b (e - b))
    (updateFirstAdjustableShim width)

resolveAdjustableShims :: FTTerm -> ComputeWidth ()
resolveAdjustableShims (FTTerm {..}) = do
  mapM_ resolveAdjustableShims children
  let lines = [begin lineSpan..end lineSpan]
  lineWidths <- mapM lineWidth lines
  let maxLineWidth = maximum lineWidths
  zipWithM_ (updateFirstAdjustableShimInLine fragmentSpan)
    lines
    (map (maxLineWidth -) lineWidths)

data ComputeHeightEnv
  = ComputeHeightEnv
  { chsFragments   ::Fragments
  , chsHierarchyLUT::IntMap IntSet
  , chsLineMap     ::IntMap (Span Exclusive Int)
  }

data ComputeHeightState
  = ComputeHeightState
  { chsDrawOrder     ::IntMap [DrawOrder]
  -- chsDrawOrder is a map from term ids to lists of spans. The order
  -- of this list is the clockwise order that the spans should be drawn.
  -- each span is paired with an double which represents the distance of this span
  -- to the upper or lower bound of the line. e.g.:
  --                    --------------    <- order is 2
  --    order is 1 ->    ------- ----     <- order is 1
  --                       foo    ab
  --                     ------------     <- order is 1
  --                    --------------    <- order is 2
  , chsInterLineSpans::Map.Map (LineNumber, Side) (IT.IntervalTree Double Double)
  }

type ComputeHeight a = ReaderT ComputeHeightEnv (State ComputeHeightState) a

lowerFlip :: Flip a -> a
lowerFlip (Flip     a) = a
lowerFlip (DontFlip a) = a

-- addInterLineSpan adds a span Above or Below `lineNumber', updating
-- chsInterLineSpans. It returns the order of the span, and the span itself.
addInterLineSpan
  :: Side
  -> LineNumber
  -> Double
  -> Flip (Span Exclusive Double)
  -> ComputeHeight DrawOrder
addInterLineSpan side lineNumber height span = do
  let key = (lineNumber, side)
  intervalTree <- gets (Map.findWithDefault IT.empty key . chsInterLineSpans)
  let b = begin . lowerFlip $ span
      e = end   . lowerFlip $ span
      overlappingSpans = IT.search (b, e) intervalTree
      maxHeight =
        if null overlappingSpans
        then 0
        else maximum . map snd $ overlappingSpans
      intervalTree' = IT.insert (b, e) (maxHeight + height) intervalTree
  modify (\s -> s { chsInterLineSpans = Map.insert key intervalTree' (chsInterLineSpans s) })
  pure $ HorzLine lineNumber side (maxHeight + height) span

-- find the span (in screen coordinates) of the term with `id' on
-- `lineNumber,' including all child fragments / gadgets / and terms.
data FindSpanByIdOnLineState
  = SearchingForBegin Double
  | Inside (Span Exclusive Double)

findSpanByIdOnLine
  :: Int
  -> LineNumber
  -> ComputeHeight (Span Exclusive Double)
findSpanByIdOnLine id lineNumber = do
  fragments <- asks chsFragments
  ancestor  <- asks (isAncestorOf . chsHierarchyLUT)
  lineSpan  <- asks (IntMap.findWithDefault (Span (0, 0))
                     (getLineNumber lineNumber) . chsLineMap)
  pure $ flip evalState (SearchingForBegin 0.0) $ do
    forAllObjectsInRangeM_ fragments lineSpan $ \cid width frag ->
      get >>= \case
      SearchingForBegin curX
        | (id == cid || cid `ancestor` id) && not (isFragmentWhitespace frag) ->
          put $ Inside (Span (curX, curX + width))
        | otherwise ->
          put $ SearchingForBegin (curX + width)
      Inside span
        | id == cid || cid `ancestor` id ->
          put $ Inside (spanExpandEnd width span)
        | otherwise ->
          put $ Inside span

    gets $ \case
      SearchingForBegin _    -> Span (0, 0)
      Inside            span -> span

computeHeightImpl :: FTTerm -> ComputeHeight ()
computeHeightImpl FTTerm {..} = do
  let paddingHeight = requiredMargin computedStyle
  mapM_ computeHeightImpl children
  let beginLine = begin lineSpan
      endLine   = end   lineSpan
  if beginLine == endLine
    then do
    span <- findSpanByIdOnLine id beginLine
    abv  <- addInterLineSpan Above beginLine paddingHeight (DontFlip span)
    blw  <- addInterLineSpan Below beginLine paddingHeight (Flip span)
    let spansInDrawOrder = [ abv, blw, ClosePath ]
    addSpansInDrawOrder spansInDrawOrder
    else do

    minLeft <- do
      lefts <- catMaybes <$> forM [beginLine..endLine]
        (\line -> do
            span <- findSpanByIdOnLine id line
            pure $ if empty span then Nothing else Just (begin span))
      pure $ if null lefts then 0 else minimum lefts

    -- Handle the first and second lines from the top-down
    firstLineSpan  <- findSpanByIdOnLine id beginLine
    secondLineSpan <- findSpanByIdOnLine id (succ beginLine)
    let secondLineSpan' = Span @Exclusive
          ( minLeft
          , if end secondLineSpan > 0
            then min (end secondLineSpan) (begin firstLineSpan)
            else begin firstLineSpan )
    abv1 <- addInterLineSpan Above beginLine paddingHeight (DontFlip firstLineSpan)
    abv2 <- addInterLineSpan Above (succ beginLine) paddingHeight (DontFlip secondLineSpan')

    -- Handle the second-to-last and last lines from the bottom-up
    lastLineSpan <- findSpanByIdOnLine id endLine
    let lastLineSpan' = Span @Exclusive
          ( minLeft
          , min (end firstLineSpan) (end lastLineSpan) )

    -- A degenerate span is one in which the span covers just two lines,
    -- and there exists a vertical line which does not interspect the span
    -- in either the first line or the second.
    --
    -- In such cases, we want to render the span as two disjoint polygons.
    let isDegenerateSpan =
          (succ beginLine == endLine) && end lastLineSpan' < begin firstLineSpan

    secondToLastLineSpan <- findSpanByIdOnLine id (pred endLine)
    let secondToLastLineSpan' = Span @Exclusive
          ( if isDegenerateSpan
            then max (end lastLineSpan') (begin secondToLastLineSpan)
            else end lastLineSpan'
          , if end secondToLastLineSpan > 0
            then min (end firstLineSpan) (end secondToLastLineSpan)
            else end firstLineSpan)
    blw1 <- addInterLineSpan Below endLine paddingHeight (Flip lastLineSpan')
    blw2 <- addInterLineSpan Below (pred endLine) paddingHeight (Flip secondToLastLineSpan')

    --                        +--abv1--+  <- draw first
    -- draw fourth ->  +-abv2-+        |
    --                 |               |
    --                 |      +--blw2--+  <- draw second
    --                 +-blw1-+           <- draw third
    let spansInDrawOrder = catMaybes
          [ Just abv1
          , Just blw2
          , if isDegenerateSpan then Just ClosePath else Nothing
          , Just blw1
          , Just abv2
          , Just ClosePath ]
    addSpansInDrawOrder spansInDrawOrder
    where addSpansInDrawOrder spans =
            modify (\s -> s { chsDrawOrder = IntMap.insert id spans (chsDrawOrder s) })

maxValueOfIntervalTree :: IT.IntervalTree a Double -> Double
maxValueOfIntervalTree = IT.foldr (max . maximum) 0

computeLineHeight :: LineNumber -> FragmentTree -> Double
computeLineHeight lineNumber tree =
  foldr (max . fragmentHeight . view _2) minimumLineHeight fragsOnLine
  where lineSpan = (IntMap.!) (lineMap tree) (getLineNumber lineNumber)
        b = begin lineSpan
        e = end lineSpan
        fragsOnLine = view (sliced b (e - b)) (fragments tree)
        minimumLineHeight = leading tree

computeInterLineSpacing
  :: FragmentTree
  -> State ComputeHeightState (V.Vector LineMetric)
computeInterLineSpacing tree = do
  interLineSpans <- gets (Map.map maxValueOfIntervalTree . chsInterLineSpans)
  let maxLineNumber = fst . fst $ Map.findMax interLineSpans
      allLineNumbers = [LineNumber 0..maxLineNumber]
  pure . V.fromListN (getLineNumber maxLineNumber + 1) . reverse . snd
    $ foldl (\(y, lines) lineNumber ->
                let aboveSpace = Map.findWithDefault 0 (lineNumber, Above) interLineSpans
                    belowSpace = Map.findWithDefault 0 (lineNumber, Below) interLineSpans
                    lineHeight = computeLineHeight lineNumber tree
                in ( y + aboveSpace + lineHeight + belowSpace
                   , LineMetric
                     { top = y
                     , aboveSpace
                     , lineHeight
                     , belowSpace
                     }:lines)
            ) (0, []) allLineNumbers

computeHeight :: FragmentTree -> (FragmentTree, LayoutMetrics)
computeHeight tree =
  let initialEnv = ComputeHeightEnv
        { chsFragments = fragments tree
        , chsHierarchyLUT = hierarchyLUT tree
        , chsLineMap = lineMap tree
        }
      initialState = ComputeHeightState
        { chsDrawOrder = IntMap.empty
        , chsInterLineSpans = Map.empty
        }
      (_, finalState) = flip runState initialState $
                        flip runReaderT initialEnv $
                        computeHeightImpl (root tree)
      interLineSpacing = evalState (computeInterLineSpacing tree) finalState
  in ( tree { interLineSpans = chsInterLineSpans finalState }
     , LayoutMetrics { interLineSpacing, drawOrder = chsDrawOrder finalState })

layout
  :: (MonadIO m, Renderable r m)
  => Double
  -> r
  -> m (LayoutMetrics, Fragments, FTTerm)
layout leading renderable = do
  initialTree <- renderableToFragmentTree leading renderable
  let (finalTree, metrics) = computeHeight . computeWidth $ initialTree
  pure ( metrics
       , fragments finalTree
       , root finalTree )
