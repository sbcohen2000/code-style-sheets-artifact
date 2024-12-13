module Tiny.CHI.Value where

import Data.List
import Data.Map ( Map )
import Tiny.CHI.Anno
import Tiny.CHI.Types

type Env = Map String Value

data Clo
  = Clo
  { clo_parameters :: [Pat TypecheckPhase]
  , clo_env :: Env
  , clo_body :: Exp TypecheckPhase
  }

data Value
  = VInt Int
  | VChar Char
  | VBool Bool
  | VClo Clo -- ^ Anonymous closures
  | VCloFix  -- ^ Mutually recursive closures
    String
    (Map String Clo)
  | VTup [Value]
  | VList [Value]
  | VIO (IO Value)

instance Eq Value where
  (VInt i) == (VInt j) = i == j
  (VChar a) == (VChar b) = a == b
  (VBool a) == (VBool b) = a == b
  (VTup as) == (VTup bs) = as == bs
  (VList as) == (VList bs) = as == bs
  _ == _ = error "bad arguments to Eq Value"

instance Ord Value where
  compare (VInt i) (VInt j) = compare i j
  compare (VChar a) (VChar b) = compare a b
  compare (VBool a) (VBool b) = compare a b
  compare (VTup as) (VTup bs) = compare as bs
  compare (VList as) (VList bs) = compare as bs
  compare _ _ = error "bad arguments to Ord Value"

instance Show Value where
  show (VInt i) = show i
  show (VChar c) = show c
  show (VBool b) = show b
  show (VClo _) = "ƒ"
  show (VCloFix _ _) = "ƒ"
  show (VTup vs) = "(" ++ intercalate ", " (map show vs) ++ ")"
  show (VList vs@((VChar _):_)) = "\"" ++ go vs ++ "\""
    where
      go [] = ""
      go ((VChar c):rest) = c:go rest
      go _ = error "invalid string in show Value"
  show (VList vs) = "[ " ++ intercalate "\n, " (map show vs) ++ " ]"
  show (VIO _) = "<io>"
