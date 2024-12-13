{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Layout.IntervalTree ( IntervalTree
                           , empty
                           , search
                           , insert
                           , foldr )
where

import Prelude hiding (foldr)

-- https://en.wikipedia.org/wiki/Interval_tree

newtype IntervalTree a b = IntervalTreeNodeImpl (IntervalTreeNode a b)

empty  :: IntervalTree a b
empty = IntervalTreeNodeImpl IntervalTreeEmpty

search :: Ord a => (a, a) -> IntervalTree a b -> [((a, a), b)]
search query (IntervalTreeNodeImpl tree) = searchImpl query tree

insert :: Ord a => (a, a) -> b -> IntervalTree a b -> IntervalTree a b
insert i v (IntervalTreeNodeImpl tree) =
  IntervalTreeNodeImpl . fst . insertImpl i v $ tree

foldr :: ([b] -> c -> c) -> c -> IntervalTree a b -> c
foldr _ c (IntervalTreeNodeImpl IntervalTreeEmpty) = c
foldr f c (IntervalTreeNodeImpl (IntervalTreeNode {..})) =
  f value (foldr f (foldr f c (IntervalTreeNodeImpl left)) (IntervalTreeNodeImpl right))

data IntervalTreeNode a b =
  IntervalTreeNode
  { left      ::IntervalTreeNode a b
  , right     ::IntervalTreeNode a b
  , interval  ::(a, a)
  , subtreeMax::a
  , value     ::[b]
  }
  | IntervalTreeEmpty
  deriving Show

overlaps :: Ord a => (a, a) -> (a, a) -> Bool
overlaps (begin1, end1) (begin2, end2) = begin1 < end2 && end1 > begin2

searchImpl :: Ord a => (a, a) -> IntervalTreeNode a b -> [((a, a), b)]
searchImpl _ IntervalTreeEmpty = []
searchImpl query@(begin, end) (IntervalTreeNode {..})  =
  let (rbegin, _rend) = interval
      leftMatches    = searchImpl query left
      rightMatches   = searchImpl query right
  in if begin > subtreeMax then []   -- query is to the right of this node and all its children
     else leftMatches
          ++ [(interval, v) | interval `overlaps` query, v <- value]
          ++ if end < rbegin then [] -- query is to the left of this node
             else rightMatches

addValue :: b -> IntervalTreeNode a b -> IntervalTreeNode a b
addValue v (IntervalTreeNode {..}) =
  IntervalTreeNode
  { left, right, interval, subtreeMax
  , value = v:value }
addValue _ IntervalTreeEmpty =
  error "IntervalTree.addValue: should not happen"

insertImpl
  :: Ord a
  => (a, a)
  -> b
  -> IntervalTreeNode a b
  -> (IntervalTreeNode a b, a)
insertImpl i v IntervalTreeEmpty =
  (IntervalTreeNode
    { left       = IntervalTreeEmpty
    , right      = IntervalTreeEmpty
    , interval   = i
    , subtreeMax = snd i
    , value      = [v] },
    snd i)
insertImpl i v root@IntervalTreeNode {..}
  | i < interval =
    let (left', leftMax) = insertImpl i v left
        newMax = max subtreeMax leftMax
    in (IntervalTreeNode
        { left = left', right, interval
        , subtreeMax = newMax
        , value }, newMax)
  | i > interval =
    let (right', rightMax) = insertImpl i v right
        newMax = max subtreeMax rightMax
    in (IntervalTreeNode
        { left, right = right', interval
        , subtreeMax = newMax
        , value }, newMax)
  | otherwise = (addValue v root, subtreeMax)
