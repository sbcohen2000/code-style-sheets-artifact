{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TS.Parser
  ( Loc(..)
  , ShallowNode(..)
  , Lang(..)
  , makeParser
  , freeParser
  , parseText
  , pprintShallowNode ) where

import Control.Monad
import Control.Applicative    (Alternative (..))
import Control.Monad.Combinators.Expr
import Data.Text              (Text)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc  (malloc, mallocBytes, free)
import Foreign.Marshal.Array  (mallocArray, peekArray, copyArray, advancePtr)
import Foreign.Ptr            (Ptr, nullPtr)
import Foreign.Storable       (peek, poke)
import qualified Data.Text             as T
import qualified TreeSitter.Haskell    as Haskell
import qualified TreeSitter.Language   as Language
import qualified TreeSitter.Node       as Node
import qualified TreeSitter.Parser     as Parser
import qualified TreeSitter.Tree       as Tree

data Loc = Loc
  { startRow   ::Int
  , startCol   ::Int
  , endRow     ::Int
  , endCol     ::Int
  , startOffset::Int
  , endOffset  ::Int }
  deriving (Eq, Show)

data Lang = Haskell

data ShallowNode =
    NamedNode     { loc::Loc, name ::Text, children::[ShallowNode] }
  | AnonymousNode { loc::Loc, value::Text, children::[ShallowNode] }
  deriving (Eq, Show)

toTSLangPtr :: Lang -> Ptr Language.Language
toTSLangPtr Haskell = Haskell.tree_sitter_haskell

makeParser :: Lang -> IO (Ptr Parser.Parser)
makeParser lang = do
  parser <- Parser.ts_parser_new
  _ <- Parser.ts_parser_set_language parser (toTSLangPtr lang)
  pure parser

freeParser :: Ptr Parser.Parser -> IO ()
freeParser = Parser.ts_parser_delete

parseText :: Ptr Parser.Parser -> Text -> IO ShallowNode
parseText parser source = do
  (str, len)  <- newCStringLen (T.unpack source)
  treePtr     <- Parser.ts_parser_parse_string parser nullPtr str len
  rootNodePtr <- malloc
  Tree.ts_tree_root_node_p treePtr rootNodePtr
  rootNode    <- peek rootNodePtr
  res         <- marshal str rootNode
  free str
  free rootNodePtr
  pure (reassociateBinops source res)

data ReassocNode
  = InfixNode
  { _infix_opSn :: ShallowNode
  , _infix_lhs :: ReassocNode
  , _infix_rhs :: ReassocNode
  , _infix_op :: Text
  }
  | LeafNode ShallowNode

data ReassocToken
  = InfixOperator ShallowNode Text
  | InfixOperand ShallowNode

newtype ReassocParser a = Parser
  { runParser :: [ReassocToken] -> Maybe (a, [ReassocToken])
  }

instance Functor ReassocParser where
  fmap f (Parser p) = Parser $ \stream -> do
    (a, stream') <- p stream
    pure (f a, stream')

instance Applicative ReassocParser where
  pure a = Parser (Just . (a,))

  Parser f <*> Parser p = Parser $ \stream -> do
    (f', stream') <- f stream
    (a, stream'') <- p stream'
    pure (f' a, stream'')

instance Monad ReassocParser where
  return = pure

  Parser p >>= k = Parser $ \stream -> do
    (a, stream') <- p stream
    let Parser p' = k a
    p' stream'

instance Alternative ReassocParser where
  empty = Parser (const Nothing)

  Parser l <|> Parser r = Parser $ \stream ->
    case l stream of
      Just (a, stream') -> Just (a, stream')
      Nothing ->
        case r stream of
          Just (a, stream') -> Just (a, stream')
          Nothing -> Nothing

instance MonadPlus ReassocParser where

pTerm :: ReassocParser ReassocNode
pTerm = Parser p
  where
    p ((InfixOperand sn):rest) = Just (LeafNode sn, rest)
    p _ = Nothing

mkOp :: Text -> ShallowNode -> ReassocNode -> ReassocNode -> ReassocNode
mkOp op sn lhs rhs = InfixNode sn lhs rhs op

pOp :: Text -> ReassocParser ShallowNode
pOp txt = Parser p
  where
    p ((InfixOperator sn op):rest)
     | op == txt = Just (sn, rest)
     | otherwise = Nothing
    p _ = Nothing

ops :: [[Operator ReassocParser ReassocNode]]
ops = [ [ InfixR (op "."), InfixR (op ">>>") ]
      , [ InfixR (op "^"), InfixR (op "^^"), InfixR (op "**") ]
      , [ InfixL (op "*"), InfixL (op "/") ]
      , [ InfixL (op "+"), InfixL (op "-") ]
      , [ InfixR (op ":"), InfixR (op "++") ]
      , [ InfixN (op "==")
        , InfixN (op "/=")
        , InfixN (op "<")
        , InfixN (op "<=")
        , InfixN (op ">=")
        , InfixN (op ">") ]
      , [ InfixL (op "<*>"), InfixL (op "<$>") ]
      , [ InfixR (op "&&") ]
      , [ InfixR (op "||") ]
      , [ InfixL (op ">>"), InfixL (op ">>=") ]
      , [ InfixR (op "=<<"), InfixR (op "<|>") ]
      , [ InfixR (op "$") ]
      ]
  where op str = mkOp str <$> pOp str

reassociateBinops :: Text -> ShallowNode -> ShallowNode
reassociateBinops source = go
  where
    go anon@AnonymousNode { children } =
      anon { children = map (reassociateBinops source) children }
    go named@NamedNode { name = "exp_infix" } =
      let subtree = infixSubtree source named
          tokens = flattenInfixSubtree subtree
          subtree' = runParser (makeExprParser pTerm ops) tokens
      in case subtree' of
        Nothing -> error "reassoc failed (parse failure)"
        Just (rns, []) -> fst $ unInfixSubtree rns
        Just (_, _:_) -> error "reassoc failed (didn't consume all tokens)"
    go named@NamedNode { children } =
      named { children = map (reassociateBinops source) children }

flattenInfixSubtree :: ReassocNode -> [ReassocToken]
flattenInfixSubtree (InfixNode opSn lhs rhs op) =
  flattenInfixSubtree lhs ++ [InfixOperator opSn op] ++ flattenInfixSubtree rhs
flattenInfixSubtree (LeafNode sn) = [InfixOperand sn]

-- | assumes a is before b in the document
fromAToB :: Loc -> Loc -> Loc
fromAToB a b =
  Loc
  { startRow = startRow a
  , startCol = startCol a
  , endRow = endRow b
  , endCol = endCol b
  , startOffset = startOffset a
  , endOffset = endOffset b
  }

infixSubtree :: Text -> ShallowNode -> ReassocNode
infixSubtree source = go
  where
    go anon@AnonymousNode {} = LeafNode (reassociateBinops source anon)
    go NamedNode
      { name = "exp_infix"
      , children = [lhs, opSn@NamedNode { loc }, rhs] } =
      InfixNode opSn (go lhs) (go rhs) op
      where
        op = sliceText loc source
    go named@NamedNode {} = LeafNode (reassociateBinops source named)

unInfixSubtree :: ReassocNode -> (ShallowNode, Loc)
unInfixSubtree (InfixNode opSn lhs rhs _) =
  ( NamedNode
    { loc = extent
    , name = "exp_infix"
    , children = [lhsSn, opSn, rhsSn]
    }, extent )
  where
    extent = fromAToB lhsLoc rhsLoc
    (lhsSn, lhsLoc) = unInfixSubtree lhs
    (rhsSn, rhsLoc) = unInfixSubtree rhs
unInfixSubtree (LeafNode sn) = (sn, loc sn)

sliceText :: Loc -> Text -> Text
sliceText loc =
  T.copy . T.take (endOffset loc - startOffset loc) . T.drop (startOffset loc)

marshal :: Ptr CChar -> Node.Node -> IO ShallowNode
marshal source node = do
  let nChildren   = fromIntegral (Node.nodeChildCount node)
      startRow    = fromIntegral (Node.pointRow    . Node.nodeStartPoint $ node)
      startCol    = fromIntegral (Node.pointColumn . Node.nodeStartPoint $ node)
      endRow      = fromIntegral (Node.pointRow    . Node.nodeEndPoint   $ node)
      endCol      = fromIntegral (Node.pointColumn . Node.nodeEndPoint   $ node)
      startOffset = fromIntegral (Node.nodeStartByte node)
      endOffset   = fromIntegral (Node.nodeEndByte   node)
      loc         = Loc { startRow, startCol, endRow, endCol, startOffset, endOffset }
  childrenPtrs <- mallocArray nChildren
  tsNode       <- malloc
  poke tsNode (Node.nodeTSNode node)
  Node.ts_node_copy_child_nodes tsNode childrenPtrs
  children     <- peekArray nChildren childrenPtrs
  shallowNodes <- mapM (marshal source) children
  res <-
    if 0 == Node.nodeIsNamed node
    then do
      let valueLen = endOffset - startOffset
      valueCStorage <- mallocBytes valueLen::IO (Ptr CChar)
      copyArray valueCStorage (advancePtr source startOffset) valueLen
      value <- T.pack <$> peekCStringLen (valueCStorage, valueLen)
      free valueCStorage
      pure AnonymousNode { loc, value, children = shallowNodes }
    else do
      name <- T.pack <$> peekCString (Node.nodeType node)
      pure NamedNode { loc, name, children = shallowNodes }
  free tsNode
  free childrenPtrs
  pure res

pprintShallowNode :: Int -> String -> ShallowNode -> String
pprintShallowNode n contents node =
  let (loc, str, children) = case node of
        NamedNode loc name children ->
          (loc, "<<named>> (" ++ T.unpack name ++ ")", children)
        AnonymousNode loc value children ->
          (loc, "<<anonymous>> \"" ++ T.unpack value ++ "\"", children)
  in
  indent ++ str ++ "\n"
  ++ indent ++ " loc:      " ++ pprintLoc loc ++ "\n"
  ++ indent ++ " contents: " ++ show (sliceContents loc contents) ++ "\n"
  ++ concatMap (pprintShallowNode (n + 1) contents) children
  where
    indent = replicate (3 * n) ' '

    pprintLoc :: Loc -> String
    pprintLoc loc = "(" ++ show (startRow loc)
                    ++ ", " ++ show (startCol loc)
                    ++ ") -- ("
                    ++ show (endRow loc)
                    ++ ", " ++ show (endCol loc) ++ ")"

    sliceContents :: Loc -> String -> String
    sliceContents loc =
      take (endOffset loc - startOffset loc) . drop (startOffset loc)
