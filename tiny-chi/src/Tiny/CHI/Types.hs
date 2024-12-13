{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tiny.CHI.Types where

import Data.Data
import Data.Bifunctor
import Data.List
import Data.Set ( Set )
import Data.Text ( Text )
import qualified Data.Text as Text

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

data Pos =
  Pos
    { row :: Int, col :: Int  -- 1-indexed
    , offset :: Int           -- 0-indexed
    } deriving (Data, Eq, Ord)

startPos = Pos 1 1 0
noPos = Pos (-1) (-1) 0

showPos r c = show r ++ ":" ++ show c

data Range =
  Range { start :: Pos, end :: Pos }
    deriving (Data, Eq, Ord)

data Located a
  = Located { getRange :: Range, dropRange :: a }
  deriving (Data)

unlocated :: a -> Located a
unlocated = Located (Range noPos noPos)

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

type TopDecl x  = Located (TopDecl_ x)
type Decl x     = Located (Decl_ x)
type Ident x    = Located (Ident_ x)
type Op x       = Located (Op_ x)
type Token tok  = Located (Token_ tok)
type Exp x      = Located (Exp_ x)
type Pat x      = Located (Pat_ x)
type Typ x      = Located (Typ_ x)
type Knd x      = Located (Knd_ x)
type DoStmt x   = Located (DoStmt_ x)

type PrefixName x = Either (Ident x) (Token "(", Op x, Token ")")

data Ident_ x = Ident (XIdent x) String

type family XIdent x

deriving instance (
  Show x,
  Show (XIdent x)
  ) => Show (Ident_ x)

deriving instance (
  Data x,
  Data (XIdent x)
  ) => Data (Ident_ x)

data Op_ x = Op (XOp x) String

type family XOp x

deriving instance (
  Show x,
  Show (XOp x)
  ) => Show (Op_ x)

deriving instance (
  Data x,
  Data (XOp x)
  ) => Data (Op_ x)

newtype Token_ tok = Token String deriving (Data, Show)

newtype DelimitedList tok a
  = DelimitedList (Maybe (DelimitedNonEmptyList tok a))
  deriving (Data, Show)

instance Functor (DelimitedList tok) where
  fmap _ (DelimitedList Nothing) = DelimitedList Nothing
  fmap f (DelimitedList (Just nel)) = DelimitedList (Just (fmap f nel))

instance Foldable (DelimitedList tok) where
  foldr _ b (DelimitedList Nothing) = b
  foldr f b (DelimitedList (Just nel)) = foldr f b nel

instance Traversable (DelimitedList tok) where
  traverse _ (DelimitedList Nothing) = pure (DelimitedList Nothing)
  traverse f (DelimitedList (Just nel)) = DelimitedList . Just <$> traverse f nel

newtype DelimitedNonEmptyList tok a
  = DelimitedNonEmptyList (a, [(Token tok, a)])
  deriving (Data, Show)

instance Functor (DelimitedNonEmptyList tok) where
  fmap f (DelimitedNonEmptyList (a, as)) =
    DelimitedNonEmptyList (f a, map (second f) as)

instance Foldable (DelimitedNonEmptyList tok) where
  foldr f b (DelimitedNonEmptyList (a, as)) =
    a `f` foldr (\(_, a) b -> f a b) b as

instance Traversable (DelimitedNonEmptyList tok) where
  traverse f (DelimitedNonEmptyList (a, as)) =
    DelimitedNonEmptyList
    <$> ((,) <$> f a <*> traverse (\(t, a) -> (t,) <$> f a) as)

--------------------------------------------------------------------------------
-- Expression

data Exp_ x
  = EInt
    (XExp x)
    Int

  | EChar
    (XExp x)
    ( Token "'"
    , Char
    , Token "'" )

  | EString
    (XExp x)
    ( Token "\""
    , String
    , Token "\"" )

  | ENeg
    (XExp x)
    ( Token "-"
    , Exp x )

  | EVar
    (XExp x)
    (Ident x)

  | EFun
    (XExp x)
    ( Token "\\"
    , [Pat x]
    , Token "->"
    , Exp x )

  | EApp
    (XExp x)
    ( Exp x
    , [Exp x] )

  | EBinop
    (XExp x)
    ( Exp x
    , Op x
    , Exp x )

  | ELet
    (XExp x)
    ( Token "let"
    , [Decl x]
    , Token "in"
    , Exp x )

  | ETuple
    (XExp x)
    ( Token "("
    , DelimitedNonEmptyList "," (Exp x)
    , Token ")" )

  | EList
    (XExp x)
    ( Token "["
    , DelimitedList "," (Exp x)
    , Token "]" )

  | EParens
    (XExp x)
    ( Token "("
    , Exp x
    , Token ")" )

  | EIf
    (XExp x)
    ( Token "if"
    , Exp x
    , Token "then"
    , Exp x
    , Token "else"
    , Exp x )

  | ECase
    (XExp x)
    ( Token "case"
    , Exp x
    , Token "of"
    , [(Pat x, Token "->", Exp x)] )

  | EDo
    (XExp x)
    ( Token "do"
    , [DoStmt x] )

  | ETypArg
    (XExp x)
    ( Token "@"
    , Typ x )

  | EAnnot
    (XExp x)
    ( Exp x
    , Token "::"
    , Typ x )

  | XExp (XXExp x)

getExpAnno :: Exp_ x -> XExp x
getExpAnno (EInt a _) = a
getExpAnno (EChar a _) = a
getExpAnno (EString a _) = a
getExpAnno (ENeg a _) = a
getExpAnno (EVar a _) = a
getExpAnno (EFun a _) = a
getExpAnno (EApp a _) = a
getExpAnno (EBinop a _) = a
getExpAnno (ELet a _) = a
getExpAnno (ETuple a _) = a
getExpAnno (EList a _) = a
getExpAnno (EParens a _) = a
getExpAnno (EIf a _) = a
getExpAnno (ECase a _) = a
getExpAnno (EDo a _) = a
getExpAnno (ETypArg a _) = a
getExpAnno (EAnnot a _) = a
getExpAnno (XExp _) = undefined

type family XExp x
type family XXExp x

deriving instance (
  Show x,
  Show (XExp x),
  Show (XXExp x),
  Show (Ident x),
  Show (Pat x),
  Show (Op x),
  Show (Typ x),
  Show (DoStmt x),
  Show (XDecl x),
  Show (XXDecl x)
  ) => Show (Exp_ x)

deriving instance (
  Data x,
  Data (XExp x),
  Data (XXExp x),
  Data (Ident x),
  Data (Pat x),
  Data (Op x),
  Data (Typ x),
  Data (DoStmt x),
  Data (XDecl x),
  Data (XXDecl x)
  ) => Data (Exp_ x)

--------------------------------------------------------------------------------
-- Pattern

data Pat_ x
  = PInt
    (XPat x)
    Int

  | PVar
    (XPat x)
    (Ident x)

  | PApp
    (XPat x)
    ( Pat x
    , [Pat x] )

  | PTuple
    (XPat x)
    ( Token "("
    , DelimitedNonEmptyList "," (Pat x)
    , Token ")" )

  | PList
    (XPat x)
    ( Token "["
    , DelimitedList "," (Pat x)
    , Token "]" )

  | PCons
    (XPat x)
    ( Pat x
    , Op x
    , Pat x )

  | PParens
    (XPat x)
    ( Token "("
    , Pat x
    , Token ")" )

  | PWild
    (XPat x)
    (Token "_")

  | XPat (XXPat x)

type family XPat x
type family XXPat x

deriving instance (
  Show x,
  Show (XPat x),
  Show (XXPat x),
  Show (Ident x),
  Show (Op x)
  ) => Show (Pat_ x)

deriving instance (
  Data x,
  Data (XPat x),
  Data (XXPat x),
  Data (Ident x),
  Data (Op x)
  ) => Data (Pat_ x)

--------------------------------------------------------------------------------
-- Declaration

data Decl_ x
  = Equation
    (XDecl x)
    ( LHS x
    , RHS x
    , Maybe (Token "where", [Decl x]) )

  | Signature
    (XDecl x)
    ( DelimitedNonEmptyList "," (PrefixName x)
    , Token "::"
    , Typ x )

  | XDecl (XXDecl x)

type family XDecl x
type family XXDecl x

type LHS x = Either (Pat x, [Pat x]) (Pat x, Op x, Pat x)
type RHS x = (Token "=", Exp x)

deriving instance (
  Data x,
  Data (XDecl x),
  Data (XXDecl x),
  Data (Ident x),
  Data (Op x),
  Data (Exp x),
  Data (Pat x),
  Data (Typ x)
  ) => Data (Decl_ x)

deriving instance (
  Show x,
  Show (XDecl x),
  Show (XXDecl x),
  Show (Ident x),
  Show (Op x),
  Show (Exp x),
  Show (Pat x),
  Show (Typ x)
  ) => Show (Decl_ x)

--------------------------------------------------------------------------------
-- Type

data Typ_ x
  = TUnit
    (XTyp x)
    ( Token "("
    , Token ")" )

  | TCon
    (XTyp x)
    (Ident x) -- ^ includes (non-operator) base types

  | TVar
    (XTyp x)
    (Ident x)

  | TFun
    (XTyp x)
    ( Typ x
    , Token "->"
    , Typ x )

  | TFunCon
    (XTyp x)
    ( Token "("
    , Token "->"
    , Token ")" )

  | TApp
    (XTyp x)
    ( Typ x
    , [Typ x] )

  | TList
    (XTyp x)
    ( Token "["
    , Typ x
    , Token "]" )

  | TListCon
    (XTyp x)
    ( Token "["
    , Token "]" )

  | TTuple
    (XTyp x)
    ( Token "("
    , DelimitedNonEmptyList "," (Typ x)
    , Token ")" )

  | TTupleCon
    (XTyp x)
    ( Token "("
    , [Token ","]
    , Token ")" )

  | TParens
    (XTyp x)
    ( Token "("
    , Typ x
    , Token ")" )

  | TForall
    (XTyp x)
    ( Token "forall"
    , [Ident x]
    , Token "."
    , Typ x )

  | TContxt
    (XTyp x)
    ( Context x
    , Typ x )

  | TAnnot
    (XTyp x)
    ( Token "("
    , Typ x
    , Token "::"
    , Knd x
    , Token ")" )

  | XTyp (XXTyp x)

type family XTyp x
type family XXTyp x

deriving instance (
  Show x,
  Show (XTyp x),
  Show (XXTyp x),
  Show (Ident x),
  Show (Typ x),
  Show (Knd x)
  ) => Show (Typ_ x)

deriving instance (
  Data x,
  Data (XTyp x),
  Data (XXTyp x),
  Data (Ident x),
  Data (Typ x),
  Data (Knd x)
  ) => Data (Typ_ x)

--------------------------------------------------------------------------------
-- Kind

data Knd_ x
  = KStar
    (XKnd x)
    (Token "*")

  | KFun
    (XKnd x)
    ( Knd x
    , Token "->"
    , Knd x )

  | KFunCon
    (XKnd x)
    ( Token "("
    , Token "->"
    , Token ")" )

  | KApp
    (XKnd x)
    ( Knd x
    , [Knd x] )

  | KParens
    (XKnd x)
    ( Token "("
    , Knd x
    , Token ")" )

  | XKnd (XXKnd x)

type family XKnd x
type family XXKnd x

deriving instance (
  Data x,
  Data (XKnd x),
  Data (XXKnd x)
  ) => Data (Knd_ x)

deriving instance (
  Show x,
  Show (XKnd x),
  Show (XXKnd x)
  ) => Show (Knd_ x)

type Context x =
  (Maybe (Token "("), DelimitedList "," (Ident x, Ident x), Maybe (Token ")"), Token "=>")

--------------------------------------------------------------------------------
-- Do Statement

data DoStmt_ x
  = DoExp
    (XDo x)
    (Exp x)

  | DoBind
    (XDo x)
    ( Pat x
    , Token "<-"
    , Exp x )

  | DoLet
    (XDo x)
    ( Token "let"
    , [Decl x] )

  | XDo (XXDo x)

type family XDo x
type family XXDo x

deriving instance (
  Data x,
  Data (XDo x),
  Data (XXDo x),
  Data (Exp x),
  Data (Pat x),
  Data (Decl x)
  ) => Data (DoStmt_ x)

deriving instance (
  Show x,
  Show (XDo x),
  Show (XXDo x),
  Show (Exp x),
  Show (Pat x),
  Show (Decl x)
  ) => Show (DoStmt_ x)

--------------------------------------------------------------------------------
-- Top Level Declaration

data TopDecl_ x
  = TypeDecl
    (XTopDecl x)
    ( Token "type"
    , Ident x
    , [Ident x]
    , Token "="
    , Typ x )

  | DataDecl
    (XTopDecl x)
    ( Token "data"
    , Ident x
    , [Ident x]
    , Token "="
    , DelimitedNonEmptyList "|" (ConstructorDecl x))

  | Decl
    (XTopDecl x)
    (Decl x)

  | XTopDecl (XXTopDecl x)

type family XTopDecl x
type family XXTopDecl x

deriving instance (
  Data x,
  Data (XTopDecl x),
  Data (XXTopDecl x),
  Data (Exp x),
  Data (Typ x),
  Data (Decl x),
  Data (Ident x)
  ) => Data (TopDecl_ x)

deriving instance (
  Show x,
  Show (XTopDecl x),
  Show (XXTopDecl x),
  Show (Exp x),
  Show (Typ x),
  Show (Decl x),
  Show (Ident x)
  ) => Show (TopDecl_ x)

type ConstructorDecl x = (Ident x, [Typ x])

--------------------------------------------------------------------------------
-- Source File

data SourceFile x
  = SourceFile
    (XSourceFile x)
    [TopDecl x]
    SourceFileInfo

type family XSourceFile x

deriving instance (
  Show x,
  Show (XSourceFile x),
  Show (TopDecl x)
  ) => Show (SourceFile x)

deriving instance (
  Data x,
  Data (XSourceFile x),
  Data (TopDecl x)
  ) => Data (SourceFile x)

type SourceFileInfo = (Text, CommentBox)
type CommentBox     = Set Range
