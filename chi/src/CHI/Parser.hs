{-# LANGUAGE DataKinds #-}
{-# OPTIONS -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module CHI.Parser where

import           Control.Applicative ((<|>))
import           Control.Monad  (forM_)
import           Data.List      (intercalate)
import           Data.Maybe     (fromJust, fromMaybe)
import qualified Data.Set       as Set
import           Data.Text      (Text)
import qualified Data.Text      as Text
import           System.IO
import           Text.Read      (readMaybe)

import           CHI.Types
import           TS.Parser      (Loc(..), ShallowNode(..))
import qualified TS.Parser      as TreeSitter

--------------------------------------------------------------------------------

parseSourceFile :: Text -> IO SourceFile
parseSourceFile text = do
  parser <- TreeSitter.makeParser TreeSitter.Haskell
  result <- TreeSitter.parseText parser text
  let (result', commentBox) = dropComments $ restructure result
  TreeSitter.freeParser parser
  putStrLn $ "\n" ++ replicate 60 '-'
  putStrLn $ "\n" ++ Text.unpack text
  putStrLn $ "\n" ++ pprintShallowNode 0 result'
  forM_ (Set.toAscList commentBox) print
  hFlush stdout
  let module_ = toModule result' text
  pure $ SourceFile module_ (text, commentBox)

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
restructure node@(AnonymousNode _ _ _) = node
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
dropComments_ box node@(AnonymousNode _ _ _)  = (node, box)
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
nonEmptySingleton a = (a, [])

singleton :: a -> DelimitedList sep a
singleton = Just . nonEmptySingleton

--------------------------------------------------------------------------------

-- NOTE: Consider Monad type TextReader = ((->) Text)

toExp :: ShallowNode -> Text -> Exp
toExp (NamedNode location name children) text =
  let range = toRange location in
  case (Text.unpack name, children) of

    ("haskell",     [e]) -> toExp e text
    ("top_splice",  [e]) -> toExp e text
    ("exp_name",    [e]) -> toExp e text
    ("exp_literal", [e]) -> toExp e text

    ("variable",    []) -> locatedEVar range $ sliceAndUnpack range text
    ("constructor", []) -> locatedEVar range $ sliceAndUnpack range text
    ("integer",     []) -> Located range $ EInt $ toInt $ sliceAndUnpack range text
    ("float",       []) -> Located range $ EFloat $ toFloatAndZeroes $ sliceAndUnpack range text
    ("char",        []) -> Located range $ toXCharOrXString EChar head '\'' range text
    ("string",      []) -> Located range $ toXCharOrXString EString id '\"' range text

    ("exp_negation", [neg, e]) ->
      Located range $ ENeg (toToken "-" neg text, toExp e text)

    ("exp_infix", [e1, op, e2]) ->
      Located range $ EBinop (toExp e1 text, toOp op text, toExp e2 text)

    ("exp_name", [lparen, op, rparen]) ->
      Located range $
        EPrefix (toToken "(" lparen text, toOp op text, toToken ")" rparen text)

    ("exp_section_left", [lparen, e1, op, rparen]) ->
      Located range $
        ESection
          ( toToken "(" lparen text
          , Left (toExp e1 text, toOp op text)
          , toToken ")" rparen text
          )

    ("exp_section_right", [lparen, op, e2, rparen]) ->
      Located range $
        ESection
          ( toToken "(" lparen text
          , Right (toOp op text, toExp e2 text)
          , toToken ")" rparen text
          )

    ("con_list", [lbrack, rbrack]) ->
      Located range $ EList (toToken "[" lbrack text, Nothing, toToken "]" rbrack text)

    ("con_unit", [lparen, rparen]) ->
      Located range $ EUnit (toToken "(" lparen text, toToken ")" rparen text)

    ("exp_list", _)  -> Located range $ toNonEmptyEList children text
    ("exp_tuple", _) -> Located range $ toETuple children text

    ("con_tuple", _) ->
      let (lparen : commas, [rparen]) = splitAtNegative 1 children in
      Located range $
        ETupleCon
          ( toToken "(" lparen text
          , map (\node -> toToken "," node text) commas
          , toToken ")" rparen text
          )

    ("exp_let_in", [node1, node2]) ->
      let (let_, decls) = toLetDecls node1 text
          (in_, e2)     = toInExp node2 text
      in Located range $ ELet (let_,  decls, in_, e2)

    ("exp_parens", [lparen, e, rparen]) ->
      Located range $
        EParens (toToken "(" lparen text, toExp e text, toToken ")" rparen text)

    ("exp_apply", e:es) ->
      Located range $ EApp (toExp e text, map (flip toExp text) es)

    ("exp_lambda", backslash:nodes) ->
      let (pats, [arrow, body]) = span (not . isToken "->") nodes in
      Located range $
        EFun
          ( toToken "\\" backslash text
          , map (flip toPat text) pats
          , toToken "->" arrow text
          , toExp body text
          )

    ("exp_cond", [if_, e1, then_, e2, else_, e3]) ->
      Located range $
        EIf
          ( toToken "if"   if_ text   , toExp e1 text
          , toToken "then" then_ text , toExp e2 text
          , toToken "else" else_ text , toExp e3 text )

    ("exp_case", [case_, scrutinee, of_, branches]) ->
      Located range $
        ECase
          ( toToken "case" case_ text
          , toExp scrutinee text
          , toToken "of" of_ text
          , toBranches branches text
          )
        where
          toBranches (NamedNode _ alts altList) text =
            case Text.unpack alts of
              "alts" -> map (flip toBranch text) altList

          toBranch (NamedNode _ alt [p, arrow, e]) text =
            case Text.unpack alt of
              "alt" -> (toPat p text, toToken "->" arrow text, toExp e text)

    ("exp_do", do_:stmts) ->
      Located range $ EDo (toToken "do" do_ text, map (flip toDoStmt text) stmts)

    ("exp_list_comprehension", _) ->
      Located range $ toEListComp children text

    ("exp_type_application", [at, t]) ->
      Located range $ ETypArg (toToken "@" at text, toTyp t text)

    ("exp_record", _) ->
      let (e : lbrace : fields, [rbrace]) = splitAtNegative 1 children in
      Located range $
        ERecord
          ( toExp e text
          , ( toToken "{" lbrace text
            , toDelimitedList "," fields text toExpField
            , toToken "}" rbrace text
            )
          )

    ("ANNOTATED", [e, dcolon, t]) ->
      Located range $ EAnnot (toExp e text, toToken "::" dcolon text, toTyp t text)

    ("empty_file", []) ->
      locatedEVar (Range startPos startPos) $ "-- Empty file. Get to it!"

    (nm, _) ->
      locatedEVar (Range startPos startPos) $
        "ERROR toExp: (" ++ nm ++ ") [" ++ Text.unpack text ++ "]"

locatedEVar :: Range -> String -> Exp
locatedEVar range str =
  Located range $ EVar $ Located range $ Ident str

toInt :: String -> Int
toInt str =
  fromMaybe (error $ "ERROR toInt: [" ++ str ++ "]") (readMaybe str)

toFloatAndZeroes :: String -> FloatAndZeroes
toFloatAndZeroes str =
  let
    float  = fromMaybe (error $ "ERROR toFloat: [" ++ str ++ "]") (readMaybe str)
    str'   = show float
    zeroes = replicate (length str - length str') '0'
  in
    (float, zeroes)

toLocatedInt :: ShallowNode -> Text -> Located Int
toLocatedInt node text =
  let range = toRange (loc node) in
  Located range $ toInt $ sliceAndUnpack range text

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

toIdent :: ShallowNode -> Text -> Ident
toIdent node@(NamedNode location name children) text =
  let range = toRange location in
  case (Text.unpack name, children) of
    ("variable",      []) -> Located range $ Ident $ sliceAndUnpack range text
    ("constructor",   []) -> Located range $ Ident $ sliceAndUnpack range text
    ("type",          []) -> Located range $ Ident $ sliceAndUnpack range text
    ("type_variable", []) -> Located range $ Ident $ sliceAndUnpack range text
    ("module", []) ->
      Located range $ Ident $ toModuleString node text
    ("qualified_module", _) ->
      Located range $ Ident $ intercalate "." $ map (flip toModuleString text) children

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

toOp :: ShallowNode -> Text -> Op
toOp node text
  | str `elem` ops = Located range $ Op str
  | otherwise      = error $ "ERROR toOp: [" ++ str ++ "]"
      where
        range  = toRange $ loc node
        str    = sliceAndUnpack range text
        ops    = [ "+",  "-",  "*",  "/"
                 ,  ":", "$", ">=", "<="
                 , "==", "/=", "&&", "||"
                 , ">>>" , ">>=", ">>", "++"
                 , "." ]

-- TODO: refactor with toXUnit, toXParens, toXTuple, toXTupleCon, toXApp, toXFun, etc.

toLetDecls :: ShallowNode -> Text -> (Token "let", [Decl])
toLetDecls (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("exp_let", [let_, decls]) -> (toToken "let" let_ text, toDecls decls text)

toDecls :: ShallowNode -> Text -> [Decl]
toDecls (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("decls", decls) -> map (fromJust . flip toDecl text) decls

toDecl :: ShallowNode -> Text -> Maybe Decl
toDecl (NamedNode location name children) text =
  Located (toRange location) <$> case (Text.unpack name, children) of
    -- Called "function" even when no args (patterns) on left-hand side
    ("function", _) ->
      Just $ Equation (lhs, rhsOrGrhss, maybeWhere)
        where
          (lhs, rest) =
            if isToken "(" (head children) then
              let ([lparen, op, rparen], rest) = splitAt 3 children in
              let range = Range (start (toRange (loc lparen))) (end (toRange (loc rparen))) in
              let prefixOp = (toToken "(" lparen text, toOp op text, toToken ")" rparen text) in
              let p = Located range $ PPrefix prefixOp in
              (Left (p, ys), rest)
            else if isNamedNode "infix" (head children) then
              let ([infix_], rest) = splitAt 1 children in
              (Right (toInfixLhs infix_ text), rest)
            else
              let ([first], rest) = splitAt 1 children in
              let p = toPat first text in
              (Left (p, ys), rest)

          (ys, rest')          = toPatternsAnd rest text
          (rhsOrGrhss, rest'') = toRhsOrGrhssAnd rest' text
          maybeWhere           = toMaybeWhere rest'' text

    ("signature", _) ->
      if length children == 3 then
        let [x, dcolon, t] = children in
        Just $ Signature
          ( nonEmptySingleton $ Left $ toIdent x text
          , toToken "::" dcolon text
          , toTyp t text
          )
      else if isToken "(" (head children) then
        let [lparen, op, rparen, dcolon, t] = children in
        Just $ Signature
          ( nonEmptySingleton $ Right (toToken "(" lparen text, toOp op text, toToken ")" rparen text)
          , toToken "::" dcolon text
          , toTyp t text
          )
      else
        -- Could also allow prefix operator symbols to be intermingled
        -- with identifiers, but meh.
        let (front, [dcolon, t]) = splitAtNegative 2 children in
        Just $ Signature
          ( toDelimitedNonEmptyList "," front text (\node -> Left . toIdent node)
          , toToken "::" dcolon text
          , toTyp t text
          )

    _ ->
      Nothing

toInExp :: ShallowNode -> Text -> (Token "in", Exp)
toInExp (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("exp_in", [in_, e2]) -> (toToken "in" in_ text, toExp e2 text)

-- toPatterns :: ShallowNode -> Text -> [Pat]
-- toPatterns (NamedNode _ name children) text =
--   case (Text.unpack name, children) of
--     ("patterns", ys) -> map (flip toPat text) ys

toPatternsAnd :: [ShallowNode] -> Text -> ([Pat], [ShallowNode])
toPatternsAnd nodes@(NamedNode _ name children : rest) text =
  case (Text.unpack name, children) of
    ("patterns", ys) -> (map (flip toPat text) ys, rest)
    _                -> ([], nodes)
toPatternsAnd nodes@(AnonymousNode _ _ _ : _) _ =
  ([], nodes)

toInfixLhs :: ShallowNode -> Text -> (Pat, Op, Pat)
toInfixLhs infix_@(NamedNode _ _ [p1, op, p2]) text
  | isNamedNode "infix" infix_ =
      (toPat p1 text, toOp op text, toPat p2 text)

toRhsOrGrhssAnd :: [ShallowNode] -> Text -> (Either RHS [GRHS], [ShallowNode])
toRhsOrGrhssAnd (eq@(AnonymousNode _ _ _) : e : rest) text | isToken "=" eq =
  (Left (toToken "=" eq text, toExp e text), rest)
toRhsOrGrhssAnd nodes text =
  (Right grhss, rest) where (grhss, rest) = toGrhssAnd nodes text

toGrhssAnd :: [ShallowNode] -> Text -> ([GRHS], [ShallowNode])
toGrhssAnd (NamedNode _ guard_equation
             [NamedNode _ guards
               [pipe, NamedNode _ guard [e1]], eq, e2] : rest) text =
  -- Not sure if or why "guards" would ever have more than one guard
  case (Text.unpack guard_equation, Text.unpack guards, Text.unpack guard) of
    ("guard_equation", "guards", "guard") -> (grhs:more, rest')
       where
         grhs = ( toToken "|" pipe text , toExp e1 text
                , toToken "=" eq text   , toExp e2 text )
         (more, rest') = toGrhssAnd rest text
toGrhssAnd nodes _ =
  ([], nodes)

toMaybeWhere :: [ShallowNode] -> Text -> Maybe (Token "where", [Decl])
toMaybeWhere [where_, decls] text =
  Just (toToken "where" where_ text, toDecls decls text)
toMaybeWhere [] _ =
  Nothing

toPat :: ShallowNode -> Text -> Pat
toPat node@(NamedNode location name children) text =
  let range = toRange location in
  case (Text.unpack name, children) of

    ("pat_name",    [p]) -> toPat p text
    ("pat_literal", [p]) -> toPat p text

    ("integer",     []) -> Located range $ PInt $ toInt $ sliceAndUnpack range text
    ("float",       []) -> Located range $ PFloat $ toFloatAndZeroes $ sliceAndUnpack range text
    ("char",        []) -> Located range $ toXCharOrXString PChar head '\'' range text
    ("string",      []) -> Located range $ toXCharOrXString PString id '\"' range text
    ("variable",    []) -> Located range $ PVar $ toIdent node text
    ("constructor", []) -> Located range $ PVar $ toIdent node text

    ("pat_wildcard", [underscore]) ->
      Located range $ PWild (toToken "_" underscore text)

    ("pat_infix", [p1, op, p2]) ->
      Located range $ PBinop (toPat p1 text, toOp op text, toPat p2 text)

    ("con_unit", [lparen, rparen]) ->
      Located range $ PUnit (toToken "(" lparen text, toToken ")" rparen text)

    ("con_list", [lbrack, rbrack]) ->
      Located range $ PList (toToken "[" lbrack text, Nothing, toToken "]" rbrack text)

    ("pat_list", _)  -> Located range $ toNonEmptyPList children text
    ("pat_tuple", _) -> Located range $ toPTuple children text

    ("pat_parens", [lparen, p, rparen]) ->
      Located range $
        PParens (toToken "(" lparen text, toPat p text, toToken ")" rparen text)

    ("pat_apply", p:ps) ->
      Located range $ PApp (toPat p text, map (flip toPat text) ps)

    ("pat_record", [con, pat_fields]) ->
      let Located _ (PVar ident) = toPat con text in
      Located range $ PRecord (ident, toPatRecord pat_fields text)

    (nm, _) ->
      error $ "ERROR toPat: (" ++ nm ++ ") [" ++ sliceAndUnpack range text ++ "]"

toTyp :: ShallowNode -> Text -> Typ
toTyp node@(NamedNode location name children) text =
  let range = toRange location in
  case (Text.unpack name, children) of

    ("type_name",    [t]) -> toTyp t text
    ("type_literal", [t]) -> toTyp t text

    ("type", [])          -> Located range $ TCon $ toIdent node text
    ("type_variable", []) -> Located range $ TVar $ toIdent node text
    ("type_tuple", _)     -> Located range $ toTTuple children text

    ("con_unit", [lparen, rparen]) ->
      Located range $ TUnit (toToken "(" lparen text, toToken ")" rparen text)

    ("type_apply", t:ts) ->
      Located range $ TApp (toTyp t text, map (flip toTyp text) ts)

    ("type_list", [lbrack, t, rbrack]) ->
      Located range $
        TList (toToken "[" lbrack text, toTyp t text, toToken "]" rbrack text)

    ("con_list", [lbrack, rbrack]) ->
      Located range $
        TListCon (toToken "[" lbrack text, toToken "]" rbrack text)

    ("fun", [t1, arrow, t2]) ->
      Located range $
        TFun (toTyp t1 text, toToken "->" arrow text, toTyp t2 text)

    ("tycon_arrow", [lparen, arrow, rparen]) ->
      Located range $
        TFunCon
          ( toToken "(" lparen text
          , toToken "->" arrow text
          , toToken ")" rparen text
          )

    ("con_tuple", _) ->
      let (lparen : commas, [rparen]) = splitAtNegative 1 children in
      Located range $
        TTupleCon
          ( toToken "(" lparen text
          , map (\node -> toToken "," node text) commas
          , toToken ")" rparen text
          )

    ("type_parens", [lparen, t, rparen]) ->
      Located range $
        TParens (toToken "(" lparen text, toTyp t text, toToken ")" rparen text)

    ("forall", [quantifiers, dot, t]) ->
      Located range $
        TForall (forall_, vars, toToken "." dot text, toTyp t text)
        where
          (forall_, vars) = toQuantifiers quantifiers text
          toQuantifiers node@(NamedNode _ _ (forall_ : xs)) text
            | isNamedNode "quantifiers" node =
                (toToken "forall" forall_ text, map (flip toIdent text) xs)

    ("context", _) ->
      Located range $ TContxt (toContext nodes text, toTyp t text)
        where
          (nodes, [t]) =
            splitAtNegative 1 children

    ( "annotated_type_variable",
      [lparen, NamedNode _ annotated [t, dcolon, k], rparen]) ->
      case Text.unpack annotated of
        "ANNOTATED" ->
          Located range $
            TAnnot
              ( toToken "(" lparen text
              , toTyp t text
              , toToken "::" dcolon text
              , toKnd k text
              , toToken ")" rparen text
              )

    _ ->
      error $ "ERROR toTyp: [" ++ sliceAndUnpack range text ++ "]"

toKnd :: ShallowNode -> Text -> Knd
toKnd node@(NamedNode location name children) text =
  let range = toRange location in
  case (Text.unpack name, children) of

    ("type_name", [k]) -> toKnd k text

    ("type_star", []) -> Located range $ KStar $ toToken "*" node text

    ("fun", [k1, arrow, k2]) ->
      Located range $
        KFun (toKnd k1 text, toToken "->" arrow text, toKnd k2 text)

    ("tycon_arrow", [lparen, arrow, rparen]) ->
      Located range $
        KFunCon
          ( toToken "(" lparen text
          , toToken "->" arrow text
          , toToken ")" rparen text
          )

    ("type_apply", k:ks) ->
      Located range $ KApp (toKnd k text, map (flip toKnd text) ks)

    ("type_parens", [lparen, k, rparen]) ->
      Located range $
        KParens (toToken "(" lparen text, toKnd k text, toToken ")" rparen text)

    _ ->
      error $ "ERROR toKnd: [" ++ sliceAndUnpack range text ++ "]"

toModule :: ShallowNode -> Text -> Module
toModule (NamedNode location name children) text =
  case (Text.unpack name, children) of
    ("haskell", _) ->
      Located (toRange location) $ Module (maybeModDecl, importDecls, topDecls)
        where
          (maybeModDecl, rest) = toModDeclAnd children text
          (importDecls, rest') = toImportDeclsAnd rest text
          topDecls             = map (flip toTopDecl text) rest'

toModDeclAnd :: [ShallowNode] -> Text -> (Maybe ModDecl, [ShallowNode])
toModDeclAnd nodes text =
  case nodes of
    (module_ : x : next : rest) ->
      if not $ isToken "module" module_ then
        (Nothing, nodes)
      else if isToken "where" next then
        (Just $ toModDecl module_ x Nothing next text, rest)
      else if isNamedNode "exports" next then
        (Just $ toModDecl module_ x (Just next) (head rest) text, tail rest)
      else
        error "toModDeclAnd"
    _ ->
      (Nothing, nodes)

toModDecl module_ x maybeExports where_ text =
  let range = Range (start (toRange (loc module_))) (end (toRange (loc where_))) in
  Located range $
    ModDecl
      ( toToken "module" module_ text
      , toIdent x text
      , flip toExports text <$> maybeExports
      , toToken "where" where_ text
      )

toExports :: ShallowNode -> Text -> Exports
toExports (NamedNode _ _ children) text = toEntities children text

toEntities :: [ShallowNode] -> Text -> Entities
toEntities nodes text =
  let (lparen : middle, [rparen]) = splitAtNegative 1 nodes in
  ( toToken "(" lparen text
  , toDelimitedList "," middle text toEntity
  , toToken ")" rparen text
  )

toEntity :: ShallowNode -> Text -> Entity
toEntity (NamedNode _ export [child]) text
  | (Text.unpack export == "export" || Text.unpack export == "import_item") =
      if isNamedNode "variable" child then
        Left $ toIdent child text
      else
        Right $ (toIdent child text, Nothing)
toEntity (NamedNode _ export [type_, NamedNode _ export_names nodes]) text
  | (Text.unpack export == "export" || Text.unpack export == "import_item")
      && isNamedNode "type" type_
      && Text.unpack export_names == "export_names" =
    Right (toIdent type_ text, Just $ toExportNames nodes text)

toExportNames :: [ShallowNode] -> Text -> ExportNames
toExportNames [lparen, all_names, rparen] text
  | isNamedNode "all_names" all_names =
      ( toToken "(" lparen text
      , Right $ toToken ".." all_names text
      , toToken ")" rparen text
      )
toExportNames nodes text =
  let (lparen : middle, [rparen]) = splitAtNegative 1 nodes in
    ( toToken "(" lparen text
    , Left $ toDelimitedList "," middle text toIdent
    , toToken ")" rparen text
    )

toImportDeclsAnd :: [ShallowNode] -> Text -> ([ImportDecl], [ShallowNode])
toImportDeclsAnd nodes text =
  (map (flip toImportDecl text) nodes1, nodes2)
    where
      (nodes1, nodes2) = span (isNamedNode "import") nodes

toImportDecl :: ShallowNode -> Text -> ImportDecl
toImportDecl (NamedNode location name (import_ : rest)) text =
  case Text.unpack name of
    "import" ->
      let
        (maybeQualified, x:rest') = toMaybeQualifiedAnd rest text
        moduleName                = toIdent x text
        (toMaybeAs, rest'')       = toMaybeAsModuleAnd rest' text
        maybeImports              = toMaybeImports rest'' text
      in
        Located (toRange location) $
          ImportDecl
            ( toToken "import" import_ text
            , maybeQualified
            , moduleName
            , toMaybeAs
            , maybeImports
            )

toMaybeQualifiedAnd :: [ShallowNode] -> Text -> (Maybe (Token "qualified"), [ShallowNode])
toMaybeQualifiedAnd (n:ns) text
  | isToken "qualified" n = (Just $ toToken "qualified" n text, ns)
  | otherwise             = (Nothing, n:ns)

toMaybeAsModuleAnd :: [ShallowNode] -> Text -> (Maybe (Token "as", Ident), [ShallowNode])
toMaybeAsModuleAnd (n1:n2:ns) text
  | isToken "as" n1 = (Just (toToken "as" n1 text, toIdent n2 text), ns)
  | otherwise       = (Nothing, n1:n2:ns)
toMaybeAsModuleAnd nodes _ =
  (Nothing, nodes)

toMaybeImports :: [ShallowNode] -> Text -> Maybe Imports
toMaybeImports [] _ = Nothing
toMaybeImports [NamedNode _ import_list nodes] text
  | Text.unpack import_list == "import_list" =
      if length nodes > 0 && isToken "hiding" (head nodes) then
        Just (Just $ toToken "hiding" (head nodes) text, toEntities (tail nodes) text)
      else
        Just (Nothing, toEntities nodes text)

toTopDecl :: ShallowNode -> Text -> TopDecl
toTopDecl node text =
  Located (toRange (loc node)) $
    fromJust ((Decl <$> toDecl node text) <|> toTypeDecl node text)

toTypeDecl :: ShallowNode -> Text -> Maybe TopDecl_
toTypeDecl (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("type_alias", (type_ : x : {- ys ++ [eq, t] -} rest)) ->
      let (ys, [eq, t]) = splitAtNegative 2 rest in
      Just $ TypeDecl
        ( toToken "type" type_ text
        , toIdent x text
        , map (flip toIdent text) ys
        , toToken "=" eq text
        , toTyp t text
        )
    ("newtype", (newtype_ : x : {- ys ++ [eq, node] -} rest)) ->
      let (ys, [eq, node]) = splitAtNegative 2 rest in
      Just $ NewtypeDecl
        ( toToken "newtype" newtype_ text
        , toIdent x text
        , map (flip toIdent text) ys
        , toToken "=" eq text
        , toNewtypeConstructorDecl node text
        )
    ("adt", (data_ : x : {- ys ++ [eq, node] -} rest)) ->
      let (ys, [eq, node]) = splitAtNegative 2 rest in
      Just $ DataDecl
        ( toToken "data" data_ text
        , toIdent x text
        , map (flip toIdent text) ys
        , toToken "=" eq text
        , toDataConstructorDecls node text
        )
    ("class", [node1, node2, node3]) ->
      Just $ toClassDecl node1 Nothing node2 node3 text
    ("class", [node1, node2, node3, node4]) ->
      Just $ toClassDecl node1 (Just node2) node3 node4 text
    ("instance", instance_ : next : rest) ->
      let
        (maybeContextNodes, rest') =
          let NamedNode _ context contextNodes = next in
          if Text.unpack context == "context" then
            (Just contextNodes, rest)
          else
            (Nothing, next : rest)
        (NamedNode _ instance_head [NamedNode _ class_name [t1], t2])
          : where_
          : decls = rest'
      in
        Just $ InstanceDecl
          ( toToken "instance" instance_ text
          , flip toContext text <$> maybeContextNodes
          , toIdent t1 text
          , toTyp t2 text
          , toToken "where" where_ text
          , map (fromJust . flip toDecl text) decls
          )
    ("fixity", [infix_, i, NamedNode _ varop [op]]) ->
      if isToken "infixl" infix_ then
        Just $ toInfixDecl InfixL "infixl" infix_ i varop op text
      else if isToken "infixr" infix_ then
        Just $ toInfixDecl InfixR "infixr" infix_ i varop op text
      else
        Just $ toInfixDecl InfixNon "infix" infix_ i varop op text
    _ ->
      Nothing

toNewtypeConstructorDecl :: ShallowNode -> Text -> ConstructorDecl
toNewtypeConstructorDecl (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("newtype_constructor", [con, t]) ->
      (toIdent con text, Left [toTyp t text])
    ("newtype_constructor", [con, lbrace, field, rbrace]) ->
      ( toIdent con text
      , Right
          ( toToken "{" lbrace text
          , singleton (toTypField field text)
          , toToken "}" rbrace text
          )
      )

toDataConstructorDecls :: ShallowNode -> Text -> (DelimitedNonEmptyList "|" ConstructorDecl)
toDataConstructorDecls (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("constructors", _) ->
      toDelimitedNonEmptyList "|" children text toDataConstructorDecl

toDataConstructorDecl :: ShallowNode -> Text -> ConstructorDecl
toDataConstructorDecl (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("data_constructor", con : ts) ->
      (toIdent con text, Left $ map (flip toTyp text) ts)
    ("data_constructor_record", [con, record_fields]) ->
      (toIdent con text, Right $ toTypRecord record_fields text)

toExpField :: ShallowNode -> Text -> (Ident, Token "=", Exp)
toExpField exp_field@(NamedNode _ _ [f, eq, e]) text
  | isNamedNode "exp_field" exp_field =
      (toIdent f text, toToken "=" eq text, toExp e text)

toPatRecord :: ShallowNode -> Text -> PatRecord
toPatRecord pat_fields@(NamedNode _ _ nodes) text
  | isNamedNode "pat_fields" pat_fields =
      let (lbrace : fields, [rbrace]) = splitAtNegative 1 nodes in
      ( toToken "{" lbrace text
      , toDelimitedList "," fields text toPatField
      , toToken "}" rbrace text
      )

toTypRecord :: ShallowNode -> Text -> TypRecord
toTypRecord record_fields@(NamedNode _ _ nodes) text
  | isNamedNode "record_fields" record_fields =
      let (lbrace : fields, [rbrace]) = splitAtNegative 1 nodes in
      ( toToken "{" lbrace text
      , toDelimitedList "," fields text toTypField
      , toToken "}" rbrace text
      )

toTypField :: ShallowNode -> Text -> (Ident, Token "::", Typ)
toTypField field@(NamedNode _ _ [annotated@(NamedNode _ _ [f, dcolon, t])]) text
  | isNamedNode "field" field &&
    isNamedNode "ANNOTATED" annotated =
      (toIdent f text, toToken "::" dcolon text, toTyp t text)

toPatField :: ShallowNode -> Text -> (Ident, Token "=", Pat)
toPatField pat_field@(NamedNode _ _ [f, eq, p]) text
  | isNamedNode "pat_field" pat_field =
      (toIdent f text, toToken "=" eq text, toPat p text)

toClassDecl
  class_
  maybeContext
  (NamedNode _ class_head [NamedNode _ class_name [c], t])
  (NamedNode _ class_body (where_:decls))
  text =
    case map Text.unpack [class_head, class_name, class_body] of
      ["class_head", "class_name", "class_body"] ->
        ClassDecl
          ( toToken "class" class_ text
          , case maybeContext of
              Nothing -> Nothing
              Just (NamedNode _ context contextNodes) ->
                case Text.unpack context of
                  "context" ->
                    Just $ toContext contextNodes text
          , toIdent c text
          , toTyp t text
          , toToken "where" where_ text
          , map (fromJust . flip toDecl text) decls
          )

toInfixDecl f str infix_ i varop op text
  | Text.unpack varop == "varop" =
      FixityDecl
        ( f $ toToken str infix_ text
        , toLocatedInt i text
        , toOp op text
        )

-- Called "stmt" in a do-block and "qual" in a list comprehension
toDoStmt :: ShallowNode -> Text -> DoStmt
toDoStmt node@(NamedNode location _ [child]) text
  | isNamedNode "stmt" node = doStmt
  | isNamedNode "qual" node = doStmt
      where
        range =
          toRange location
        doStmt =
          if isNamedNode "bind_pattern" child then
            Located range $ toDoBind child text
          else if isNamedNode "let" child then
            Located range $ toDoLet child text
          else
            Located range $ DoExp $ toExp child text

toDoBind :: ShallowNode -> Text -> DoStmt_
toDoBind (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("bind_pattern", [p, bind, e]) ->
      DoBind (toPat p text, toToken "<-" bind text, toExp e text)

toDoLet :: ShallowNode -> Text -> DoStmt_
toDoLet (NamedNode _ name children) text =
  case (Text.unpack name, children) of
    ("let", [let_, decls]) ->
      DoLet (toToken "let" let_ text, toDecls decls text)

toEListComp :: [ShallowNode] -> Text -> Exp_
toEListComp (lbrack : e : pipe : {- delimitedStmts ++ [rbrack] -} rest) text =
  let (delimitedStmts, [rbrack]) = splitAtNegative 1 rest in
  EListComp
    ( toToken "[" lbrack text
    , toExp e text
    , toToken "|" pipe text
    , toDelimitedNonEmptyList "," delimitedStmts text toDoStmt
    , toToken "]" rbrack text
    )

toContext :: [ShallowNode] -> Text -> Context
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

toConstraints :: [ShallowNode] -> Text -> DelimitedList "," (Ident, Ident)
toConstraints ns text = toDelimitedList "," ns text toConstraint

-- TODO: rework class_name stuff
toConstraint :: ShallowNode -> Text -> (Ident, Ident)
toConstraint
  (NamedNode _ constraint [ NamedNode _ class_name [t1]
                          , NamedNode _ type_name  [t2] ]) text =
  case (Text.unpack constraint, Text.unpack class_name, Text.unpack type_name) of
    ("constraint", "class_name", "type_name") ->
      (toIdent t1 text, toIdent t2 text)

--------------------------------------------------------------------------------

type BracketedDelimitedNonEmptyList open close sep a =
  (Token open, DelimitedNonEmptyList sep a, Token close)

toNonEmptyEList :: [ShallowNode] -> Text -> Exp_
toNonEmptyEList = toNonEmptyXList EList toExp

toETuple :: [ShallowNode] -> Text -> Exp_
toETuple = toXTuple ETuple toExp

toNonEmptyPList :: [ShallowNode] -> Text -> Pat_
toNonEmptyPList = toNonEmptyXList PList toPat

toPTuple :: [ShallowNode] -> Text -> Pat_
toPTuple = toXTuple PTuple toPat

toTTuple :: [ShallowNode] -> Text -> Typ_
toTTuple = toXTuple TTuple toTyp

toNonEmptyXList xList toX nodes text =
  xList $ mapSnd3 Just $
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
  (toA (head nodes) text, iter (tail nodes))
    where
      iter []            = []
      iter (sep_:a:rest) = (toToken sep sep_ text, toA a text) : iter rest

toDelimitedList
  :: String -> [ShallowNode] -> Text -> (ShallowNode -> Text -> a)
  -> DelimitedList sep a
toDelimitedList _ [] _ _ = Nothing
toDelimitedList sep nodes text toA =
  Just $ toDelimitedNonEmptyList sep nodes text toA
