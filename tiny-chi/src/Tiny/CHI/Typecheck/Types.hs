module Tiny.CHI.Typecheck.Types where

import Data.IntSet ( IntSet )
import Data.List
import Data.Data
import qualified Data.IntSet as IntSet

data TcTyp
  = TcCon String
  | TcApp TcTyp [TcTyp]
  | TcVar Int
  deriving (Eq, Show, Data)

data TcForall = TcForall IntSet TcTyp
  deriving Show

monotype :: TcTyp -> TcForall
monotype = TcForall IntSet.empty

funType :: [TcTyp] -> TcTyp -> TcTyp
funType paramTys retTy =
  foldr (\a b -> TcApp (TcCon "Function") [a, b]) retTy paramTys

isFunType :: TcTyp -> Bool
isFunType (TcApp (TcCon "Function") [_, _]) = True
isFunType _ = False

tupType :: [TcTyp] -> TcTyp
tupType = TcApp (TcCon "Tuple")

listType :: TcTyp -> TcTyp
listType = TcApp (TcCon "List") . singleton

intType :: TcTyp
intType = TcCon "Int"

charType :: TcTyp
charType = TcCon "Char"

stringType :: TcTyp
stringType = listType charType

boolType :: TcTyp
boolType = TcCon "Bool"

ioType :: TcTyp -> TcTyp
ioType = TcApp (TcCon "IO") . singleton
