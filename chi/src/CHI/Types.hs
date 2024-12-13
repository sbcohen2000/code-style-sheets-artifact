{-# LANGUAGE DataKinds #-}

module CHI.Types where

import           Control.Applicative
import           Control.Monad.State
import           Data.Data
import           Data.List
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as Text

import StylishText
import StylishText.Utils

--------------------------------------------------------------------------------
-- Misc utils

doubleton x y = [x, y]

cons2 x y zs  = x:y:zs

uncurry3 f (a,b,c) = f a b c

mapSnd3 f (a,b,c) = (a, f b, c)

snoc xs x = xs ++ [x]

--------------------------------------------------------------------------------

sliceAndUnpackLabelled :: Range -> Text -> (Pos, [(Pos, Char)])
sliceAndUnpackLabelled range text =
  mapAccumL
  (\pos@(Pos row col offset) c ->
      case c of
        '\n' -> (Pos (row + 1) 1 (offset + 1),   (pos, c))
        '\r' -> (Pos (row + 1) 1 (offset + 1),   (pos, c))
        _    -> (Pos row (col + 1) (offset + 1), (pos, c)))
  (start range)
  (sliceAndUnpack range text)

sliceAndUnpack :: Range -> Text -> String
sliceAndUnpack range =
  Text.unpack . slice (offset (start range)) (offset (end range))

slice :: Int -> Int -> Text -> Text
slice i j =
  Text.take (j-i) . Text.drop i

--------------------------------------------------------------------------------

data Pos =
  Pos
    { row :: Int, col :: Int  -- 1-indexed
    , offset :: Int           -- 0-indexed
    } deriving (Data, Eq, Ord)

data Range =
  Range { start :: Pos, end :: Pos }
    deriving (Data, Eq, Ord)

data Located a =
  Located { getRange :: Range, dropRange :: a }
    deriving (Data)

-- dummyPos   = Pos (-1) (-1) (-1)
-- dummyRange = Range dummyPos dummyPos
-- unLocated  = Located dummyRange

startPos   = Pos 1 1 0

showPos r c = show r ++ ":" ++ show c

instance Show Pos where
  show (Pos r c _) = showPos r c

instance Show Range where
  show (Range (Pos r1 c1 _) (Pos r2 c2 _))
    | r1 == r2 && c1 == c2-1 = showPos r1 c1
    | r1 == r2               = showPos r1 c1 ++ "-" ++ show (c2-1)
    | otherwise              = showPos r1 c1 ++ "-" ++ showPos r2 (c2-1)

instance Show a => Show (Located a) where
  show (Located range a) =
    "(" ++ show a ++ ")[" ++ show range ++ "]"
    -- "[" ++ show range ++ "](" ++ show a ++ ")"

--------------------------------------------------------------------------------
-- Haskell-ish AST

data SourceFile =
  SourceFile Module SourceFileInfo deriving (Data, Show)

data Module_ =
  Module (Maybe ModDecl, [ImportDecl], [TopDecl]) deriving (Data, Show)

type SourceFileInfo = (Text, CommentBox)
type CommentBox     = Set Range

type Module     = Located Module_
type ModDecl    = Located ModDecl_
type ImportDecl = Located ImportDecl_
type TopDecl    = Located TopDecl_
type Decl       = Located Decl_
type Ident      = Located Ident_
type Op         = Located Op_
type Token tok  = Located (Token_ tok)
type Exp        = Located Exp_
type Pat        = Located Pat_
type Typ        = Located Typ_
type Knd        = Located Knd_
type DoStmt     = Located DoStmt_

-- https://typeclasses.com/identifiers-and-operators
type PrefixName = Either Ident (Token "(", Op, Token ")")

data Ident_     = Ident String deriving (Data, Show)
data Op_        = Op    String deriving (Data, Show)
data Token_ tok = Token String deriving (Data, Show)

type DelimitedList tok a         = Maybe (DelimitedNonEmptyList tok a)
type DelimitedNonEmptyList tok a = (a, [(Token tok, a)])

type FloatAndZeroes = (Float, String)

-- Note: Leading tokens (e.g. "let", "[") are (probably) not strictly
-- necessary to keep in the concrete syntax tree.

data Exp_
  = EInt Int
  | EFloat FloatAndZeroes
  | EString (Token "\"", String, Token "\"")
  | EChar (Token "\'", Char, Token "\'")
  | EVar Ident
  | EFun (Token "\\", [Pat], Token "->", Exp)
  | EApp (Exp, [Exp])
  | ENeg (Token "-", Exp)
  | EBinop (Exp, Op, Exp)
  | EPrefix (Token "(", Op, Token ")")
  | ESection (Token "(", Either (Exp, Op) (Op, Exp), Token ")")
  | EUnit (Token "(", Token ")")
  | ELet (Token "let", [Decl], Token "in", Exp)
  | EList (Token "[", DelimitedList "," Exp, Token "]")
  | ETuple (Token "(", DelimitedNonEmptyList "," Exp, Token ")")
  | ETupleCon (Token "(", [Token ","], Token ")")
  | EParens (Token "(", Exp, Token ")")
  | EIf (Token "if", Exp, Token "then", Exp, Token "else", Exp)
  | ECase (Token "case", Exp, Token "of", [(Pat, Token "->", Exp)])
  | EDo (Token "do", [DoStmt])
  | EListComp (Token "[", Exp, Token "|", DelimitedNonEmptyList "," DoStmt, Token "]")
  | ETypArg (Token "@", Typ)
  | EAnnot (Exp, Token "::", Typ)
  | ERecord (Exp, ExpRecord)
      deriving (Data, Show)

data Pat_
  = PInt Int
  | PFloat FloatAndZeroes
  | PString (Token "\"", String, Token "\"")
  | PChar (Token "\'", Char, Token "\'")
  | PVar Ident
  | PApp (Pat, [Pat])
  | PBinop (Pat, Op, Pat)
  | PPrefix (Token "(", Op, Token ")")
  | PUnit (Token "(", Token ")")
  | PTuple (Token "(", DelimitedNonEmptyList "," Pat, Token ")")
  -- tree-sitter doesn't support this?
  -- | PTupleCon (Token "(", [Token ","], Token ")")
  | PList (Token "[", DelimitedList "," Pat, Token "]")
  | PParens (Token "(", Pat, Token ")")
  | PWild (Token "_")
  | PRecord (Ident, PatRecord)
      deriving (Data, Show)

-- "Nested declarations" which may be used in any declaration list
-- (https://www.haskell.org/onlinereport/haskell2010/haskellch4.html)
data Decl_
  = Equation (LHS, Either RHS [GRHS], Maybe (Token "where", [Decl]))
  | Signature (DelimitedNonEmptyList "," PrefixName, Token "::", Typ)
      deriving (Data, Show)

type LHS  = Either (Pat, [Pat]) (Pat, Op, Pat)
type RHS  = (Token "=", Exp)
type GRHS = (Token "|", Exp, Token "=", Exp)

data ModDecl_
  = ModDecl (Token "module", Ident, Maybe Exports, Token "where")
      deriving (Data, Show)

-- https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1050005.3.4
data ImportDecl_
  = ImportDecl
      ( Token "import"
      , Maybe (Token "qualified")
      , Ident
      , Maybe (Token "as", Ident)
      , Maybe Imports
      ) deriving (Data, Show)

-- "The various items being imported into and exported out of a module are
-- called _entities_." (https://www.haskell.org/tutorial/modules.html)
type Entities = (Token "(", DelimitedList "," Entity, Token ")")
type Entity   = Either Ident (Ident, Maybe ExportNames)
type Exports  = Entities
type Imports  = (Maybe (Token "hiding"), Entities)
type ExportNames =
  (Token "(", Either (DelimitedList "," Ident) (Token ".."), Token ")")

data TopDecl_
  = TypeDecl (Token "type", Ident, [Ident], Token "=", Typ)
  | NewtypeDecl (Token "newtype", Ident, [Ident], Token "=", ConstructorDecl)
  | DataDecl (Token "data", Ident, [Ident], Token "=", DelimitedNonEmptyList "|" ConstructorDecl)
  | ClassDecl (Token "class", Maybe Context, Ident, Typ, Token "where", [Decl])
                                                      -- ^^^^^^^^^^^^^^^^^^^^^
                                                      -- actually should be Maybe
  | InstanceDecl (Token "instance", Maybe Context, Ident, Typ, Token "where", [Decl])
  | FixityDecl (InfixToken, Located Int, Op)
  | Decl Decl
      deriving (Data, Show)

type ConstructorDecl = (Ident, Either [Typ] TypRecord)
type TypRecord = (Token "{", DelimitedList "," (Ident, Token "::", Typ), Token "}")
type ExpRecord = (Token "{", DelimitedList "," (Ident, Token "=", Exp), Token "}")
type PatRecord = (Token "{", DelimitedList "," (Ident, Token "=", Pat), Token "}")

data InfixToken
  = InfixL (Token "infixl")
  | InfixR (Token "infixr")
  | InfixNon (Token "infix")
      deriving (Data, Show)

data Typ_
  = TUnit (Token "(", Token ")")
  | TCon Ident -- includes (non-operator) base types
  | TVar Ident
  | TFun (Typ, Token "->", Typ)
  | TFunCon (Token "(", Token "->", Token ")")
  | TApp (Typ, [Typ])
  | TList (Token "[", Typ, Token "]")
  | TListCon (Token "[", Token "]")
  | TTuple (Token "(", DelimitedNonEmptyList "," Typ, Token ")")
  | TTupleCon (Token "(", [Token ","], Token ")")
  | TParens (Token "(", Typ, Token ")")
  | TForall (Token "forall", [Ident], Token ".", Typ)
  | TContxt (Context, Typ)
  | TAnnot (Token "(", Typ, Token "::", Knd, Token ")")
  | TErr String
      deriving (Data, Show)

data Knd_
  = KStar (Token "*")
  | KFun (Knd, Token "->", Knd)
  | KFunCon (Token "(", Token "->", Token ")")
  | KApp (Knd, [Knd])
  | KParens (Token "(", Knd, Token ")")
      deriving (Data, Show)

type Context =
  (Maybe (Token "("), DelimitedList "," (Ident, Ident), Maybe (Token ")"), Token "=>")

data DoStmt_
  = DoExp Exp
  | DoBind (Pat, Token "<-", Exp)
  | DoLet (Token "let", [Decl])
      deriving (Data, Show)

--------------------------------------------------------------------------------
-- State Pos monad

-- sequenceA @[] @(State Pos) @StylishText
--  :: [State Pos StylishText] -> State Pos [StylishText]

-- traverse @[] @(State Pos)
--   :: (a -> State Pos b) -> [a] -> State Pos [b]

class StylishCode t where
  emitStylish :: SourceFileInfo -> (Path, t) -> State Pos StylishText

-- Note: A very modest optimization might be to put the CommentBox
-- in the State type, removing comments as they are rendered. Would
-- also keep the comments (the Ranges) in an ascending list for
-- faster retrieval. But overall, unlikely to be worth it.

instance Stylish SourceFile where
  showStylish path arg =
    let (module_, (_, env)) = subvaluesSourceFile (path, arg) in
    fst $ runState (emitStylish env module_) startPos

subvaluesSourceFile
  :: Pathed SourceFile -> (Pathed Module, Pathed SourceFileInfo)
subvaluesSourceFile x@(_, SourceFile _1 _2) = subvalues2 $ konst (_1,_2) x

-- This is the version before the CommentBox environment:
--
-- class StatefulStylish t where
--   emitStylish :: (Path, t) -> State Pos StylishText
--
-- instance Stylish Module where -- free instance
--   showStylish = defaultShowStylish
--
-- defaultShowStylish :: StatefulStylish t => Path -> t -> StylishText
-- defaultShowStylish path t =
--   fst $ runState (emitStylish (path, t)) $ startPos

--------------------------------------------------------------------------------

-- incrementPosWithLeaf str pos@(Pos r c off)
--   | str == ""             = pos
--   | all (== '\n') str     = Pos (r + length str) 1 (off + length str)
--   | not $ '\n' `elem` str = Pos r (c + length str) (off + length str)
--   | otherwise             = crash
--       where
--         crash = error $ "incrementPosWithLeaf [" ++ str ++ "] " ++ show pos

incrementPosWithLeaf str pos@(Pos r c off)
  | str == ""      = pos
  | newlines == [] = Pos r (c + length str) (off + length str)
  | otherwise      = Pos (r + length newlines)
                         (1 + length (drop (last newlines + 1) str))
                         (off + length str)
      where
        newlines = elemIndices '\n' str

emitLeafAndMove :: Maybe Path -> Classes -> Styles -> String -> State Pos StylishText
emitLeafAndMove mp cs styles str = state $ \pos0 ->
  (Node mp cs styles [TextLeaf str], incrementPosWithLeaf str pos0)

emitWhitespaceLeaf :: String -> State Pos StylishText
emitWhitespaceLeaf = emitLeafAndMove Nothing [] [("white-space", "pre", 0)]

emitCommentLeaf :: Classes -> String -> State Pos StylishText
emitCommentLeaf cls = emitLeafAndMove Nothing cls [("white-space", "pre", 0)]

emitLeaf :: ClassName -> Pathed String -> State Pos StylishText
emitLeaf tag (path, str) = emitLeafAndMove (Just path) [tag] [] str

emitNode
  :: StylishCode a
  => SourceFileInfo -> ClassName -> Path -> Pathed a -> State Pos StylishText
emitNode env tag path tuple =
  Node (Just path) [tag] [] <$> singleton <$> emitStylish env tuple

emitSpaces :: Int -> State Pos StylishText
emitSpaces n = emitWhitespaceLeaf $ replicate n ' '

emitBreaks :: Int -> State Pos StylishText
emitBreaks n = emitWhitespaceLeaf $ replicate n '\n'

emitNothing :: State Pos StylishText
emitNothing = emitSpaces 0

--------------------------------------------------------------------------------

instance (Show a, StylishCode a) => StylishCode (Located a) where
  -- This Show constraint is for debugging purposes.
  emitStylish env arg =
    let ((_,range), a) = subvaluesLocated arg in
    liftA2 prependWhitespace
      (emitWhitespaceTo env (Just (show a)) $ start range)
      (emitStylish env a)

subvaluesLocated :: Pathed (Located a) -> (Pathed Range, Pathed a)
subvaluesLocated x@(_, Located _1 _2) = subvalues2 $ konst (_1,_2) x

emitWhitespaceTo
  :: SourceFileInfo -> Maybe String -> Pos -> State Pos [StylishText]
emitWhitespaceTo env@(_, commentBox) maybeDebugStr target@(Pos r2 c2 _) = do
  current@(Pos r1 c1 _) <- get
  if r1 > r2 || (r1 == r2 && c1 > c2) then
    error $
      "emitWhitespace from " ++ show current ++ " to " ++ show target ++
      "[" ++ show maybeDebugStr ++ "]"
  else if r1 == r2 && c1 == c2 then
    pure []
  else
    case commentsInRange commentBox (Range current target) of
      [] ->
        -- TODO: Think about whether we really want/need to
        -- explicitly separate breaks and spaces.
        if r1 == r2 then
          singleton <$> emitSpaces (c2-c1)
        else
          doubleton <$> emitBreaks (r2-r1) <*> emitSpaces (c2-1)
      ranges ->
        liftA2 (++)
          (concat <$> (sequenceA $ map (emitComment env) ranges))
          (emitWhitespaceTo env Nothing target)

rangeContainedIn :: Range -> Range -> Bool
rangeContainedIn outer inner =
  start outer <= start inner && end inner <= end outer

commentsInRange :: CommentBox -> Range -> [Range]
commentsInRange commentBox outer =
  Set.toAscList $ Set.filter (rangeContainedIn outer) commentBox

emitComment :: SourceFileInfo -> Range -> State Pos [StylishText]
emitComment env@(sourceText, _) range =
  liftA2 snoc
    (emitWhitespaceTo env Nothing (start range))
    (emitCommentLeaf ["comment"] (sliceAndUnpack range sourceText))

prependWhitespace :: [StylishText] -> StylishText -> StylishText
prependWhitespace [] node = node
prependWhitespace wsLeaves node@(Node _ _ _ _) =
  Node Nothing [] [] $ wsLeaves ++ [node]
prependWhitespace _ _ = undefined

--------------------------------------------------------------------------------
-- Boilerplate

konst = fmap . const

subvals :: Pathed a -> Pathed a
subvals = subvalues1

instance StylishCode Ident_ where
  emitStylish _ (path, Ident str) = emitLeaf "ident" (path, str)

instance StylishCode Op_ where
  emitStylish _ (path, Op op) = emitLeaf "op" (path, op)

instance StylishCode (Token_ tok) where
  emitStylish _ (path, Token str) = emitLeaf "token" (path, str)

instance StylishCode Module_ where
  emitStylish env v@(p, Module _1) = emitNode env "module " p $ subvals $ konst _1 v

-- TODO: Added this for precedence in fixity declarations. Maybe re-work.
instance StylishCode Int where
  emitStylish _ (p, i) = emitLeaf "blah" (p, show i)

-- TODO: Added this for EFloat/PFloat. Maybe re-work.
instance StylishCode Float where
  emitStylish _ (p, i) = emitLeaf "blah" (p, show i)

instance StylishCode Char where
  emitStylish _ (p, c) = emitLeaf "char" (p, [c])

instance StylishCode Exp_ where
  emitStylish _     (p, EInt       i) = emitLeaf "exp" (p, show i)
  emitStylish env v@(p, EFloat    _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, EChar     _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, EString   _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, EVar      _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, EFun      _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, EApp      _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, ENeg      _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, EBinop    _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, EPrefix   _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, ESection  _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, EUnit     _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, ELet      _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, EList     _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, ETuple    _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, ETupleCon _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, EParens   _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, EIf       _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, ECase     _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, EDo       _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, EListComp _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, ETypArg   _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, EAnnot    _1) = emitNode env "exp" p $ subvals $ konst _1 v
  emitStylish env v@(p, ERecord   _1) = emitNode env "exp" p $ subvals $ konst _1 v

instance StylishCode Pat_ where
  emitStylish _     (p, PInt     i) = emitLeaf "pat" (p, show i)
  emitStylish env v@(p, PFloat  _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PChar   _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PString _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PVar    _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PApp    _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PBinop  _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PPrefix _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PUnit   _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PTuple  _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PList   _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PParens _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PWild   _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PRecord _1) = emitNode env "pat" p $ subvals $ konst _1 v

instance StylishCode Decl_ where
  emitStylish env v@(p, Equation  _1) = emitNode env "decl" p $ subvals $ konst _1 v
  emitStylish env v@(p, Signature _1) = emitNode env "decl" p $ subvals $ konst _1 v

instance StylishCode ModDecl_ where
  emitStylish env v@(p, ModDecl _1) = emitNode env "mod-decl" p $ subvals $ konst _1 v

instance StylishCode ImportDecl_ where
  emitStylish env v@(p, ImportDecl _1) = emitNode env "import-decl" p $ subvals $ konst _1 v

instance StylishCode TopDecl_ where
  emitStylish env v@(p, TypeDecl    _1) = emitNode env "top-decl" p $ subvals $ konst _1 v
  emitStylish env v@(p, NewtypeDecl _1) = emitNode env "top-decl" p $ subvals $ konst _1 v
  emitStylish env v@(p, DataDecl    _1) = emitNode env "top-decl" p $ subvals $ konst _1 v
  emitStylish env v@(p, ClassDecl   _1) = emitNode env "top-decl" p $ subvals $ konst _1 v
  emitStylish env v@(p, InstanceDecl _1) = emitNode env "top-decl" p $ subvals $ konst _1 v
  emitStylish env v@(p, FixityDecl _1)  = emitNode env "top-decl" p $ subvals $ konst _1 v
  emitStylish env v@(p, Decl        _1) = emitNode env "top-decl" p $ subvals $ konst _1 v

instance StylishCode InfixToken where
  emitStylish env v@(_, InfixL   _1) = emitStylish env $ subvals $ konst _1 v
  emitStylish env v@(_, InfixR   _1) = emitStylish env $ subvals $ konst _1 v
  emitStylish env v@(_, InfixNon _1) = emitStylish env $ subvals $ konst _1 v

instance StylishCode DoStmt_ where
  emitStylish env v@(p, DoExp  _1) = emitNode env "do-stmt" p $ subvals $ konst _1 v
  emitStylish env v@(p, DoBind _1) = emitNode env "do-stmt" p $ subvals $ konst _1 v
  emitStylish env v@(p, DoLet  _1) = emitNode env "do-stmt" p $ subvals $ konst _1 v

instance StylishCode Typ_ where
  emitStylish env v@(p, TUnit   _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TCon    _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TVar    _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TFun    _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TFunCon _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TApp    _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TList   _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TListCon _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TTuple  _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TTupleCon  _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TParens _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TForall _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TContxt _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TAnnot  _1) = emitNode env "typ" p $ subvals $ konst _1 v

  emitStylish _ (path, TErr err) = do
    -- TODO: Temporary hack
    let str = "() {- ERROR: " ++ err ++ " -}"
    let off = length str - length err
    text <- emitLeaf "typ" (path, str)
    Pos r c n <- get
    put $ Pos r (c-off) (n-off)
    pure text

instance StylishCode Knd_ where
  emitStylish env v@(p, KStar   _1) = emitNode env "knd" p $ subvals $ konst _1 v
  emitStylish env v@(p, KFun    _1) = emitNode env "knd" p $ subvals $ konst _1 v
  emitStylish env v@(p, KFunCon _1) = emitNode env "knd" p $ subvals $ konst _1 v
  emitStylish env v@(p, KApp    _1) = emitNode env "knd" p $ subvals $ konst _1 v
  emitStylish env v@(p, KParens _1) = emitNode env "knd" p $ subvals $ konst _1 v

--------------------------------------------------------------------------------

-- TODO: For primitive tuples and lists, we may want to add pathedNodes.
-- Otherwise, matching those subvalues will not be connected to stylish text.

instance StylishCode () where
  emitStylish _ _ = emitNothing

instance StylishCode a => StylishCode [a] where
  emitStylish env arg =
    Node Nothing [] [] <$> (sequenceA $ map (emitStylish env) $ subvaluesN arg)

instance StylishCode a => StylishCode (Maybe a) where
  emitStylish _     (_, Nothing) = emitNothing
  emitStylish env v@(_, Just _1) = emitStylish env $ subvals $ konst _1 v

instance (StylishCode a, StylishCode b) => StylishCode (Either a b) where
  emitStylish env v@(_, Left  _1) = emitStylish env $ subvals $ konst _1 v
  emitStylish env v@(_, Right _1) = emitStylish env $ subvals $ konst _1 v

instance (StylishCode a1, StylishCode a2) => StylishCode (a1, a2) where
  emitStylish env arg =
    let (_1, _2) = subvalues2 arg in
    Node Nothing [] [] <$> (sequenceA [emitStylish env _1, emitStylish env _2])

instance (StylishCode a1, StylishCode a2, StylishCode a3) => StylishCode (a1, a2, a3) where
  emitStylish env arg =
    let (_1, _2, _3) = subvalues3 arg in
    Node Nothing [] [] <$> (sequenceA [emitStylish env _1, emitStylish env _2, emitStylish env _3])

instance (StylishCode a1, StylishCode a2, StylishCode a3, StylishCode a4) => StylishCode (a1, a2, a3, a4) where
  emitStylish env arg =
    let (_1, _2, _3, _4) = subvalues4 arg in
    Node Nothing [] [] <$> (sequenceA [emitStylish env _1, emitStylish env _2, emitStylish env _3, emitStylish env _4])

instance (StylishCode a1, StylishCode a2, StylishCode a3, StylishCode a4, StylishCode a5) => StylishCode (a1, a2, a3, a4, a5) where
  emitStylish env arg =
    let (_1, _2, _3, _4, _5) = subvalues5 arg in
    Node Nothing [] [] <$> (sequenceA [emitStylish env _1, emitStylish env _2, emitStylish env _3, emitStylish env _4, emitStylish env _5])

instance (StylishCode a1, StylishCode a2, StylishCode a3, StylishCode a4, StylishCode a5, StylishCode a6) => StylishCode (a1, a2, a3, a4, a5, a6) where
  emitStylish env arg =
    let (_1, _2, _3, _4, _5, _6) = subvalues6 arg in
    Node Nothing [] [] <$> (sequenceA [emitStylish env _1, emitStylish env _2, emitStylish env _3, emitStylish env _4, emitStylish env _5, emitStylish env _6])
