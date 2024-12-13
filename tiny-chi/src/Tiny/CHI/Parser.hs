{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Tiny.CHI.Parser
  ( parseSourceFile
  , parseDecl
  , parseExp
  , pprintShallowNode
  ) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.List ( intercalate )
import Data.Maybe ( fromJust, fromMaybe )
import Data.Text ( Text )
import System.IO
import Text.Read ( readMaybe )
import qualified Data.Set as Set
import qualified Data.Text as Text

import           Tiny.CHI.Types
import           Tiny.CHI.Anno
import           TS.Parser      (Loc(..), ShallowNode(..))
import qualified TS.Parser      as TreeSitter

mapSnd3 f (a,b,c) = (a, f b, c)

--------------------------------------------------------------------------------

parseAs :: (ShallowNode -> CommentBox -> a) -> Text -> IO a
parseAs f text = do
  parser <- TreeSitter.makeParser TreeSitter.Haskell
  result <- TreeSitter.parseText parser text
  let (result', commentBox) = dropComments $ restructure result
  TreeSitter.freeParser parser
  forM_ (Set.toAscList commentBox) print
  hFlush stdout
  pure $ f result' commentBox

parseSourceFile :: Text -> IO (SourceFile ParsePhase)
parseSourceFile text =
  parseAs (\root commentBox ->
             SourceFile () (toHaskellDecls root text) (text, commentBox)) text

parseDecl :: Text -> IO (TopDecl ParsePhase)
parseDecl text =
  parseAs (\root _ -> toHaskellDecl root text) text

parseExp :: Text -> IO (Exp ParsePhase)
parseExp text =
  parseAs (\root _ -> toHaskellExp root text) text

--------------------------------------------------------------------------------

pprintShallowNode :: Int -> ShallowNode -> String
pprintShallowNode n node =
  let (loc, str, children) = case node of
        TreeSitter.NamedNode loc name children ->
          (loc, Text.unpack name, children)
        TreeSitter.AnonymousNode loc value children ->
          (loc, "\"" ++ Text.unpack value ++ "\"", children)
  in
  indent ++ str ++ " " ++ gray ++ pprintLoc loc ++ reset ++ "\n"
  ++ concatMap (pprintShallowNode (n + 1)) children
  where indent = replicate (3 * n) ' '
        gray   = "\ESC[90m"
        reset  = "\ESC[0m"

        pprintLoc :: TreeSitter.Loc -> String
        pprintLoc loc = "(" ++ show (TreeSitter.startRow loc)
                        ++ ", " ++ show (TreeSitter.startCol loc)
                        ++ ") -- ("
                        ++ show (TreeSitter.endRow loc)
                        ++ ", " ++ show (TreeSitter.endCol loc) ++ ") "
                        ++ show (TreeSitter.startOffset loc) ++ "-"
                        ++ show (TreeSitter.endOffset loc)

--------------------------------------------------------------------------------

-- Restructure type-annotated expressions (e :: t)
-- and kind-annotated types (t :: k) into single nodes.
restructure :: ShallowNode -> ShallowNode
restructure node@(AnonymousNode {}) = node
restructure node@(NamedNode l name children)
  | isNamedNode "signature" node = node
  | otherwise = NamedNode l name $ coalesce $ map restructure children
    where
      coalesce :: [ShallowNode] -> [ShallowNode]
      coalesce (obj : dcolon : ann : rest)
        | isToken "::" dcolon = newNode : coalesce rest
        | otherwise           = obj : coalesce (dcolon : ann : rest)
            where
              newNode =
                NamedNode newLoc (Text.pack "ANNOTATED") [obj, dcolon, ann]
              newLoc =
                Loc
                  { startRow    = startRow $ loc obj
                  , startCol    = startCol $ loc obj
                  , endRow      = endRow $ loc ann
                  , endCol      = endCol $ loc ann
                  , startOffset = startOffset $ loc obj
                  , endOffset   = endOffset $ loc ann
                  }
      coalesce (x:rest) = x : coalesce rest
      coalesce []       = []

dropComments :: ShallowNode -> (ShallowNode, CommentBox)
dropComments = dropComments_ Set.empty

dropComments_ :: CommentBox -> ShallowNode -> (ShallowNode, CommentBox)
dropComments_ box node@(AnonymousNode {})  = (node, box)
dropComments_ box (NamedNode l name children) = (NamedNode l name children', box')
  where
    (children', box') =
      foldr dropIfComment ([], box) children
    dropIfComment child (accChildren, accBox)
      | isNamedNode "comment" child =
          (accChildren, Set.insert (toRange (loc child)) accBox)
      | otherwise =
          let (child', accBox') = dropComments_ accBox child in
          (child' : accChildren, Set.union accBox accBox')

--------------------------------------------------------------------------------

-- NOTE: Maybe pick a single representation (from TS.Parser or CHI/Types)
toRange :: TreeSitter.Loc -> Range
toRange loc =
  Range start end
    where
      start = Pos (1 + startRow loc) (1 + startCol loc) (startOffset loc)
      end   = Pos (1 + endRow loc) (1 + endCol loc) (endOffset loc)

-- NOTE: Could make this better by returning Maybe children.
isNamedNode s (NamedNode _ name _) = Text.unpack name == s
isNamedNode _ _                    = False

splitAtNegative :: Int -> [a] -> ([a], [a])
splitAtNegative i list = splitAt (length list - i) list

nonEmptySingleton :: a -> DelimitedNonEmptyList tok a
nonEmptySingleton a = DelimitedNonEmptyList (a, [])

--------------------------------------------------------------------------------

toXCharOrXString xCharOrString toCharOrString delim range text =
  let
    (finalPos, chars)        = fmap (fmap fst) (sliceAndUnpackLabelled range text)
    (open : middle, [close]) = splitAtNegative 1 chars
    openQuoteLoc             = Range open (head middle)
    stringLoc                = Range (head middle) close
    closeQuoteLoc            = Range close finalPos
  in
    xCharOrString
      ( Located openQuoteLoc $ Token $ sliceAndUnpack openQuoteLoc text
      , toCharOrString $ sliceAndUnpack stringLoc text
      , Located closeQuoteLoc $ Token $ sliceAndUnpack closeQuoteLoc text
      )

-- NOTE: Consider Monad type TextReader = ((->) Text)

toExp :: ShallowNode -> Text -> Exp ParsePhase
toExp (NamedNode location name children) text =
  let range = toRange location in
  case (Text.unpack name, children) of

    ("haskell",     [e]) -> toExp e text
    ("top_splice",  [e]) -> toExp e text
    ("exp_name",    [e]) -> toExp e text
    ("exp_literal", [e]) -> toExp e text

    ("variable",    []) -> locatedEVar range $ sliceAndUnpack range text
    ("constructor", []) -> locatedEVar range $ sliceAndUnpack range text
    ("integer",     []) -> Located range $ EInt () $ toInt $ sliceAndUnpack range text
    ("float",       []) -> error "unimplemented (float)"
    ("char",        []) -> Located range $ toXCharOrXString (EChar ()) head '\'' range text
    ("string",      []) -> Located range $ toXCharOrXString (EString ()) id '\"' range text

    ("exp_negation", [neg, e]) ->
      Located range $ ENeg () (toToken "-" neg text, toExp e text)

    ("exp_infix", [e1, op, e2]) ->
      Located range $ EBinop () (toExp e1 text, toOp op text, toExp e2 text)

    ("exp_name", _) -> error "unimplemented (exp_name)"
    ("exp_section_left", _) -> error "unimplemented (exp_section_left)"
    ("exp_section_right", _) -> error "unimplemented (exp_section_right)"

    ("con_list", [lbrack, rbrack]) ->
      Located range $ EList () (toToken "[" lbrack text, DelimitedList Nothing, toToken "]" rbrack text)

    ("con_unit", _) -> error "unimplemented (con_unit)"
    ("exp_list", _)  -> Located range $ toNonEmptyEList children text
    ("exp_tuple", _) -> Located range $ toETuple children text
    ("con_tuple", _) -> error "unimplemented (con_tuple)"

    ("exp_let_in", [node1, node2]) ->
      let (let_, decls) = toLetDecls node1 text
          (in_, e2)     = toInExp node2 text
      in Located range $ ELet () (let_,  decls, in_, e2)

    ("exp_parens", [lparen, e, rparen]) ->
      Located range $
        EParens () (toToken "(" lparen text, toExp e text, toToken ")" rparen text)

    ("exp_apply", e:es) ->
      Located range $ EApp () (toExp e text, map (`toExp` text) es)

    ("exp_lambda", backslash:nodes) ->
      let (pats, [arrow, body]) = span (not . isToken "->") nodes in
      Located range $
        EFun ()
          ( toToken "\\" backslash text
          , map (`toPat` text) pats
          , toToken "->" arrow text
          , toExp body text
          )

    ("exp_cond", [if_, e1, then_, e2, else_, e3]) ->
      Located range $
        EIf ()
          ( toToken "if"   if_ text   , toExp e1 text
          , toToken "then" then_ text , toExp e2 text
          , toToken "else" else_ text , toExp e3 text )

    ("exp_case", [case_, scrutinee, of_, branches]) ->
      Located range $
        ECase ()
          ( toToken "case" case_ text
          , toExp scrutinee text
          , toToken "of" of_ text
          , toBranches branches text
          )
        where
          toBranches (NamedNode _ alts altList) text =
            case Text.unpack alts of
              "alts" -> map (`toBranch` text) altList

          toBranch (NamedNode _ alt [p, arrow, e]) text =
            case Text.unpack alt of
              "alt" -> (toPat p text, toToken "->" arrow text, toExp e text)

    ("exp_do", do_:stmts) ->
      Located range $ EDo () (toToken "do" do_ text, map (`toDoStmt` text) stmts)

    ("exp_list_comprehension", _) ->
      error "unimplemented (exp_list_comprehension)"

    ("exp_type_application", [at, t]) ->
      Located range $ ETypArg () (toToken "@" at text, toTyp t text)

    ("exp_record", _) -> error "unimplemented (exp_record)"

    ("ANNOTATED", [e, dcolon, t]) ->
      Located range $ EAnnot () (toExp e text, toToken "::" dcolon text, toTyp t text)

    ("empty_file", []) ->
      locatedEVar (Range startPos startPos) "-- Empty file. Get to it!"

    _ -> Located range (XExp (ErrorExp "parse error"))

locatedEVar :: Range -> String -> Exp ParsePhase
locatedEVar range str =
  Located range $ EVar () $ Located range $ Ident () str

toInt :: String -> Int
toInt str = fromMaybe (error $ "ERROR toInt: [" ++ str ++ "]") (readMaybe str)

toIdent :: ShallowNode -> Text -> Ident ParsePhase
toIdent node@(NamedNode location name children) text =
  let range = toRange location in
  case (Text.unpack name, children) of
    ("variable",      []) -> Located range $ Ident () $ sliceAndUnpack range text
    ("constructor",   []) -> Located range $ Ident () $ sliceAndUnpack range text
    ("type",          []) -> Located range $ Ident () $ sliceAndUnpack range text
    ("type_variable", []) -> Located range $ Ident () $ sliceAndUnpack range text
    ("module", []) ->
      Located range $ Ident () $ toModuleString node text
    ("qualified_module", _) ->
      Located range $ Ident () $ intercalate "." $ map (`toModuleString` text) children

    _ ->
      error $ "ERROR toIdent: [" ++ sliceAndUnpack range text ++ "]"
toIdent (AnonymousNode location _ _) text =
  let range = toRange location in
  error $ "ERROR toIdent: (AnonymousNode) [" ++ sliceAndUnpack range text ++ "]"
  ++ " (" ++ show location ++ ")"

toModuleString :: ShallowNode -> Text -> String
toModuleString (NamedNode location name children) text =
  case (Text.unpack name, children) of
    ("module", []) -> sliceAndUnpack (toRange location) text

toToken :: String -> ShallowNode -> Text -> Token tok
toToken str node text =
  let range = toRange $ loc node in
  if isToken str node then
    Located range $ Token $ sliceAndUnpack range text
  else
   error $ "ERROR toToken \"" ++ str ++ "\" [" ++ sliceAndUnpack range text ++ "]"

-- Some tokens are represented as a `NamedNode` rather than `AnonymousNode`.
isToken :: String -> ShallowNode -> Bool
isToken s (AnonymousNode _ val _) = Text.unpack val == s
isToken s (NamedNode _ name _)    = (s, Text.unpack name) `elem` namedTokens
  where
    namedTokens =
      [ (",", "comma")
      , ("where", "where")
      , ("..", "all_names")
      , ("*", "type_star")
      ]

toOp :: ShallowNode -> Text -> Op ParsePhase
toOp node text
  | str `elem` ops = Located range $ Op () str
  | otherwise      = error $ "ERROR toOp: [" ++ str ++ "]"
      where
        range  = toRange $ loc node
        str    = sliceAndUnpack range text
        ops    = [ "+",  "-",  "*",  "/"
                 ,  ":", "$", ">=", "<="
                 , "<", ">", "==", "/="
                 , "&&", "||", "++", ">>="
                 , ">>>", "=<<", "." ]

-- TODO: refactor with toXUnit, toXParens, toXTuple, toXTupleCon, toXApp, toXFun, etc.

toLetDecls :: ShallowNode -> Text -> (Token "let", [Decl ParsePhase])
toLetDecls (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("exp_let", [let_, decls]) -> (toToken "let" let_ text, toDecls decls text)

toDecls :: ShallowNode -> Text -> [Decl ParsePhase]
toDecls (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("decls", decls) -> map (fromJust . flip toDecl text) decls

toInfixLhs :: ShallowNode -> Text -> (Pat ParsePhase, Op ParsePhase, Pat ParsePhase)
toInfixLhs infix_@(NamedNode _ _ [p1, op, p2]) text
  | isNamedNode "infix" infix_ =
      (toPat p1 text, toOp op text, toPat p2 text)

toDecl :: ShallowNode -> Text -> Maybe (Decl ParsePhase)
toDecl (NamedNode location name children) text =
  Located (toRange location) <$> case (Text.unpack name, children) of
    -- Called "function" even when no args (patterns) on left-hand side
    ("function", _) ->
      Just $ Equation () (lhs, rhs, maybeWhere)
        where
          (lhs, rest) =
            if isToken "(" (head children) then
              error "unsupported (function)"
            else if isNamedNode "infix" (head children) then
              let ([infix_], rest) = splitAt 1 children in
                (Right (toInfixLhs infix_ text), rest)
            else
              let ([first], rest) = splitAt 1 children in
              let p = toPat first text in
                (Left (p, ys), rest)

          (ys, rest')          = toPatternsAnd rest text
          (rhs, rest'') = toRhsAnd rest' text
          maybeWhere           = toMaybeWhere rest'' text

    ("signature", _) ->
      if length children == 3 then
        let [x, dcolon, t] = children in
        Just $ Signature ()
          ( nonEmptySingleton $ Left $ toIdent x text
          , toToken "::" dcolon text
          , toTyp t text
          )
      else if isToken "(" (head children) then
        let [lparen, op, rparen, dcolon, t] = children in
        Just $ Signature ()
          ( nonEmptySingleton $ Right (toToken "(" lparen text, toOp op text, toToken ")" rparen text)
          , toToken "::" dcolon text
          , toTyp t text
          )
      else
        -- Could also allow prefix operator symbols to be intermingled
        -- with identifiers, but meh.
        let (front, [dcolon, t]) = splitAtNegative 2 children in
        Just $ Signature ()
          ( toDelimitedNonEmptyList "," front text (\node -> Left . toIdent node)
          , toToken "::" dcolon text
          , toTyp t text
          )

    _ ->
      Nothing

toInExp :: ShallowNode -> Text -> (Token "in", Exp ParsePhase)
toInExp (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("exp_in", [in_, e2]) -> (toToken "in" in_ text, toExp e2 text)

-- toPatterns :: ShallowNode -> Text -> [Pat]
-- toPatterns (NamedNode _ name children) text =
--   case (Text.unpack name, children) of
--     ("patterns", ys) -> map (flip toPat text) ys

toPatternsAnd :: [ShallowNode] -> Text -> ([Pat ParsePhase], [ShallowNode])
toPatternsAnd nodes@(NamedNode _ name children : rest) text =
  case (Text.unpack name, children) of
    ("patterns", ys) -> (map (`toPat` text) ys, rest)
    _                -> ([], nodes)
toPatternsAnd nodes@(AnonymousNode {} : _) _ =
  ([], nodes)

toRhsAnd :: [ShallowNode] -> Text -> (RHS ParsePhase, [ShallowNode])
toRhsAnd (eq@(AnonymousNode {}) : e : rest) text | isToken "=" eq =
  ((toToken "=" eq text, toExp e text), rest)
toRhsAnd _ _  = error "unimplemented (GRHS)"

toMaybeWhere :: [ShallowNode] -> Text -> Maybe (Token "where", [Decl ParsePhase])
toMaybeWhere [where_, decls] text =
  Just (toToken "where" where_ text, toDecls decls text)
toMaybeWhere [] _ =
  Nothing

toPat :: ShallowNode -> Text -> Pat ParsePhase
toPat node@(NamedNode location name children) text =
  let range = toRange location in
  case (Text.unpack name, children) of

    ("pat_name",    [p]) -> toPat p text
    ("pat_literal", [p]) -> toPat p text

    ("integer",     []) -> Located range $ PInt () $ toInt $ sliceAndUnpack range text
    ("float",       []) -> error "unimplemented (float)"
    ("char",        []) -> error "unimplemented (char)"
    ("string",      []) -> error "unimplemented (string)"
    ("variable",    []) -> Located range $ PVar () $ toIdent node text
    ("constructor", []) -> Located range $ PVar () $ toIdent node text

    ("pat_wildcard", [underscore]) ->
      Located range $ PWild () (toToken "_" underscore text)

    ("pat_infix", [p1, op, p2]) ->
      if
        let (Located _ (Op _ str)) = toOp op text
        in str == ":"
      then Located range $ PCons () (toPat p1 text, toOp op text, toPat p2 text)
      else error "unimplemented (pat_infix)"
    ("con_unit", _) -> error "unimplemented (con_unit)"

    ("con_list", [lbrack, rbrack]) ->
      Located range $ PList () (toToken "[" lbrack text, DelimitedList Nothing, toToken "]" rbrack text)

    ("pat_list", _)  -> Located range $ toNonEmptyPList children text
    ("pat_tuple", _) -> Located range $ toPTuple children text

    ("pat_parens", [lparen, p, rparen]) ->
      Located range $
        PParens () (toToken "(" lparen text, toPat p text, toToken ")" rparen text)

    ("pat_apply", p:ps) ->
      Located range $ PApp () (toPat p text, map (`toPat` text) ps)

    ("pat_record", _) -> error "unimplemented"

    _ -> Located range (XPat (ErrorPat "parse error"))

toTyp :: ShallowNode -> Text -> Typ ParsePhase
toTyp node@(NamedNode location name children) text =
  let range = toRange location in
  case (Text.unpack name, children) of

    ("type_name",    [t]) -> toTyp t text
    ("type_literal", [t]) -> toTyp t text

    ("type", [])          -> Located range $ TCon () $ toIdent node text
    ("type_variable", []) -> Located range $ TVar () $ toIdent node text
    ("type_tuple", _)     -> Located range $ toTTuple children text

    ("con_unit", [lparen, rparen]) ->
      Located range $ TUnit () (toToken "(" lparen text, toToken ")" rparen text)

    ("type_apply", t:ts) ->
      Located range $ TApp () (toTyp t text, map (`toTyp` text) ts)

    ("type_list", [lbrack, t, rbrack]) ->
      Located range $
        TList () (toToken "[" lbrack text, toTyp t text, toToken "]" rbrack text)

    ("con_list", [lbrack, rbrack]) ->
      Located range $
        TListCon () (toToken "[" lbrack text, toToken "]" rbrack text)

    ("fun", [t1, arrow, t2]) ->
      Located range $
        TFun () (toTyp t1 text, toToken "->" arrow text, toTyp t2 text)

    ("tycon_arrow", [lparen, arrow, rparen]) ->
      Located range $
        TFunCon ()
          ( toToken "(" lparen text
          , toToken "->" arrow text
          , toToken ")" rparen text
          )

    ("con_tuple", _) ->
      let (lparen : commas, [rparen]) = splitAtNegative 1 children in
      Located range $
        TTupleCon ()
          ( toToken "(" lparen text
          , map (\node -> toToken "," node text) commas
          , toToken ")" rparen text
          )

    ("type_parens", [lparen, t, rparen]) ->
      Located range $
        TParens () (toToken "(" lparen text, toTyp t text, toToken ")" rparen text)

    ("forall", [quantifiers, dot, t]) ->
      Located range $
        TForall () (forall_, vars, toToken "." dot text, toTyp t text)
        where
          (forall_, vars) = toQuantifiers quantifiers text
          toQuantifiers node@(NamedNode _ _ (forall_ : xs)) text
            | isNamedNode "quantifiers" node =
                (toToken "forall" forall_ text, map (`toIdent` text) xs)

    ("context", _) ->
      Located range $ TContxt () (toContext nodes text, toTyp t text)
        where
          (nodes, [t]) =
            splitAtNegative 1 children

    ( "annotated_type_variable",
      [lparen, NamedNode _ annotated [t, dcolon, k], rparen]) ->
      case Text.unpack annotated of
        "ANNOTATED" ->
          Located range $
            TAnnot ()
              ( toToken "(" lparen text
              , toTyp t text
              , toToken "::" dcolon text
              , toKnd k text
              , toToken ")" rparen text
              )

    _ -> Located range (XTyp (ErrorTyp "parse error"))

toKnd :: ShallowNode -> Text -> Knd ParsePhase
toKnd node@(NamedNode location name children) text =
  let range = toRange location in
  case (Text.unpack name, children) of

    ("type_name", [k]) -> toKnd k text

    ("type_star", []) -> Located range $ KStar () $ toToken "*" node text

    ("fun", [k1, arrow, k2]) ->
      Located range $
        KFun () (toKnd k1 text, toToken "->" arrow text, toKnd k2 text)

    ("tycon_arrow", [lparen, arrow, rparen]) ->
      Located range $
        KFunCon ()
          ( toToken "(" lparen text
          , toToken "->" arrow text
          , toToken ")" rparen text
          )

    ("type_apply", k:ks) ->
      Located range $ KApp () (toKnd k text, map (`toKnd` text) ks)

    ("type_parens", [lparen, k, rparen]) ->
      Located range $
        KParens () (toToken "(" lparen text, toKnd k text, toToken ")" rparen text)

    _ -> Located range (XKnd (ErrorKnd "parse error"))

toHaskellDecls :: ShallowNode -> Text -> [TopDecl ParsePhase]
toHaskellDecls (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("haskell", _) -> map (`toTopDecl` text) children

toHaskellDecl :: ShallowNode -> Text -> TopDecl ParsePhase
toHaskellDecl (NamedNode location name children) text =
  case (Text.unpack name, children) of
    ("haskell", _) -> toTopDecl (head children) text
    _ -> Located (toRange location) $ XTopDecl (ErrorTopDecl "parse error")

toHaskellExp :: ShallowNode -> Text -> Exp ParsePhase
toHaskellExp (NamedNode location name children) text =
  case (Text.unpack name, children) of
    ("haskell", _) -> toExp (head children) text
    _ -> Located (toRange location) $ XExp (ErrorExp "parse error")

toTopDecl :: ShallowNode -> Text -> TopDecl ParsePhase
toTopDecl node text =
  Located (toRange (loc node)) $
    fromJust ((Decl () <$> toDecl node text) <|> toTypeDecl node text)

toTypeDecl :: ShallowNode -> Text -> Maybe (TopDecl_ ParsePhase)
toTypeDecl (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("type_alias", type_ : x : {- ys ++ [eq, t] -} rest) ->
      let (ys, [eq, t]) = splitAtNegative 2 rest in
      Just $ TypeDecl ()
        ( toToken "type" type_ text
        , toIdent x text
        , map (`toIdent` text) ys
        , toToken "=" eq text
        , toTyp t text
        )
    ("newtype", _) -> error "unimplemented (newtype)"
    ("adt", data_ : x : {- ys ++ [eq, node] -} rest) ->
      let (ys, [eq, node]) = splitAtNegative 2 rest in
      Just $ DataDecl ()
        ( toToken "data" data_ text
        , toIdent x text
        , map (`toIdent` text) ys
        , toToken "=" eq text
        , toDataConstructorDecls node text
        )
    ("class", _) -> error "unimplemented (class)"
    ("instance", _) -> error "unimplemented (instance)"
    ("fixity", _) -> error "unimplemented (fixity)"
    _ ->
      Nothing

toDataConstructorDecls
  :: ShallowNode
  -> Text
  -> DelimitedNonEmptyList "|" (ConstructorDecl ParsePhase)
toDataConstructorDecls (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("constructors", _) ->
      toDelimitedNonEmptyList "|" children text toDataConstructorDecl

toDataConstructorDecl :: ShallowNode -> Text -> ConstructorDecl ParsePhase
toDataConstructorDecl (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("data_constructor", con : ts) ->
      (toIdent con text, map (`toTyp` text) ts)
    ("data_constructor_record", _) ->
      error "unimplemented (data_constructor_record)"

-- Called "stmt" in a do-block and "qual" in a list comprehension
toDoStmt :: ShallowNode -> Text -> DoStmt ParsePhase
toDoStmt node@(NamedNode location _ [child]) text
  | isNamedNode "stmt" node = doStmt
  | isNamedNode "qual" node = doStmt
      where
        range =
          toRange location
        doStmt
          | isNamedNode "bind_pattern" child =
            Located range $ toDoBind child text
          | isNamedNode "let" child =
            Located range $ toDoLet child text
          | otherwise =
            Located range $ DoExp () $ toExp child text

toDoBind :: ShallowNode -> Text -> DoStmt_ ParsePhase
toDoBind (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("bind_pattern", [p, bind, e]) ->
      DoBind () (toPat p text, toToken "<-" bind text, toExp e text)

toDoLet :: ShallowNode -> Text -> DoStmt_ ParsePhase
toDoLet (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("let", [let_, decls]) ->
      DoLet () (toToken "let" let_ text, toDecls decls text)

toContext :: [ShallowNode] -> Text -> Context ParsePhase
toContext nodes@(lparen:rest) text
  | isToken "(" lparen =
      let (constraints, [rparen, arrow]) = splitAtNegative 2 rest in
      ( Just $ toToken "(" lparen text
      , toConstraints constraints text
      , Just $ toToken ")" rparen text
      , toToken "=>" arrow text
      )
  | otherwise =
      let (constraints, [arrow]) = splitAtNegative 1 nodes in
      (Nothing, toConstraints constraints text, Nothing, toToken "=>" arrow text)

toConstraints
  :: [ShallowNode]
  -> Text
  -> DelimitedList "," (Ident ParsePhase, Ident ParsePhase)
toConstraints ns text = toDelimitedList "," ns text toConstraint

-- TODO: rework class_name stuff
toConstraint :: ShallowNode -> Text -> (Ident ParsePhase, Ident ParsePhase)
toConstraint
  (NamedNode _ constraint [ NamedNode _ class_name [t1]
                          , NamedNode _ type_name  [t2] ]) text =
  case (Text.unpack constraint, Text.unpack class_name, Text.unpack type_name) of
    ("constraint", "class_name", "type_name") ->
      (toIdent t1 text, toIdent t2 text)

--------------------------------------------------------------------------------

type BracketedDelimitedNonEmptyList open close sep a =
  (Token open, DelimitedNonEmptyList sep a, Token close)

toNonEmptyEList :: [ShallowNode] -> Text -> Exp_ ParsePhase
toNonEmptyEList = toNonEmptyXList (EList ()) toExp

toETuple :: [ShallowNode] -> Text -> Exp_ ParsePhase
toETuple = toXTuple (ETuple ()) toExp

toNonEmptyPList :: [ShallowNode] -> Text -> Pat_ ParsePhase
toNonEmptyPList = toNonEmptyXList (PList ()) toPat

toPTuple :: [ShallowNode] -> Text -> Pat_ ParsePhase
toPTuple = toXTuple (PTuple ()) toPat

toTTuple :: [ShallowNode] -> Text -> Typ_ ParsePhase
toTTuple = toXTuple (TTuple ()) toTyp

toNonEmptyXList xList toX nodes text =
  xList $ mapSnd3 (DelimitedList . Just) $
    toBracketedDelimitedNonEmptyList "[" "]" "," nodes text toX

toXTuple xTuple toX nodes text =
  xTuple $
    toBracketedDelimitedNonEmptyList "(" ")" "," nodes text toX

toBracketedDelimitedNonEmptyList
  :: String -> String -> String
  -> [ShallowNode]
  -> Text
  -> (ShallowNode -> Text -> a)
  -> BracketedDelimitedNonEmptyList open close sep a
toBracketedDelimitedNonEmptyList open close sep nodes text toA =
  (tok1, list, tok2)
    where
      tok1   = toToken open (head nodes) text
      tok2   = toToken close (last nodes) text
      list   = toDelimitedNonEmptyList sep middle text toA
      middle = take (length nodes - 2) $ drop 1 nodes

toDelimitedNonEmptyList
  :: String
  -> [ShallowNode]
  -> Text
  -> (ShallowNode -> Text -> a)
  -> DelimitedNonEmptyList sep a
toDelimitedNonEmptyList sep nodes text toA =
  DelimitedNonEmptyList (toA (head nodes) text, iter (tail nodes))
    where
      iter []            = []
      iter (sep_:a:rest) = (toToken sep sep_ text, toA a text) : iter rest

toDelimitedList
  :: String -> [ShallowNode] -> Text -> (ShallowNode -> Text -> a)
  -> DelimitedList sep a
toDelimitedList _ [] _ _ = DelimitedList Nothing
toDelimitedList sep nodes text toA =
  DelimitedList $ Just $ toDelimitedNonEmptyList sep nodes text toA
