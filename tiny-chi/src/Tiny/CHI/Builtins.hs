{-# LANGUAGE LambdaCase #-}
module Tiny.CHI.Builtins ( builtins, builtinsGamma ) where

import Data.List
import Data.Map ( Map )
import System.Random ( randomRIO )
import Tiny.CHI.Typecheck.Types
import Tiny.CHI.Value
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

asString :: [Value] -> String
asString (VChar c:rst) = c:asString rst
asString []            = []
asString _             = error "bug in tc (asString)"

builtins :: Map String ([Value] -> Value, TcForall)
builtins = Map.fromList
  [ ( "print_"
    , (\case [v] -> VIO (print v >> pure (VInt 0))
             _ -> error "bug in tc (print)"
      , TcForall (IntSet.singleton 0) (funType [TcVar 0] (ioType intType))))
  , ( "pure"
    , (\case [v] -> VIO (pure v)
             _ -> error "bug in tc (pure)"
      , TcForall (IntSet.singleton 0) (funType [TcVar 0] (ioType (TcVar 0)))))
  , ( "randomRIO"
    , (\case [VTup [VInt l, VInt r]] -> VIO (VInt <$> randomRIO (l, r))
             _ -> error "bug in tc (randomRIO)"
      , TcForall IntSet.empty (funType [tupType [intType, intType]] (ioType intType))))
  , ( "fmap"
    , (\case _ -> error "TODO"
             _ -> error "bug in tc (fmap)"
      , TcForall (IntSet.fromList [0, 1]) (funType [funType [TcVar 0] (TcVar 1), ioType (TcVar 0)] (ioType (TcVar 1)))))
  , ("lines_"
    , (\case [VList cs] -> VList (map (VList . map VChar) (lines (asString cs)))
             _ -> error "bug in tc (lines)"
      , TcForall IntSet.empty (funType [stringType] (listType stringType))))
  , ( "isPrefixOf_"
    , (\case [VList pfx, VList cs] -> (VBool (asString pfx `isPrefixOf` asString cs))
             _ -> error "bug in tc (assertEq)"
      , TcForall IntSet.empty (funType [stringType, stringType] boolType)))
  , ("getContents_"
    , (\case [VInt _] -> VIO (VList . map VChar <$> getContents)
             _ -> error "bug in tc (getContents)"
      , TcForall IntSet.empty (funType [intType] (ioType stringType))))
  , ( "==", compareBinop (==))
  , ( "<", compareBinop (<))
  , ( ">", compareBinop (>))
  , ( "<=", compareBinop (<=))
  , ( ">=", compareBinop (>=))
  , ( "/=", compareBinop (/=))
  , ( "+", intBinop "+" (+))
  , ( "-", intBinop "-" (-))
  , ( "*", intBinop "*" (*))
  , ( "mod", intBinop "mod" mod)
  , ( "div", intBinop "div" div)
  , ( ":",
      (\case [v, VList vs] -> VList (v:vs)
             _ -> error "bug in tc (:)"
      , TcForall (IntSet.singleton 0) (funType [TcVar 0, listType (TcVar 0)] (listType (TcVar 0)))))
  , ( "head"
    , (\case [VList v] -> head v
             _ -> error "bug in tc (head)"
      , TcForall (IntSet.singleton 0) (funType [listType (TcVar 0)] (TcVar 0))))
  , ( "tail"
    , (\case [VList v] -> VList (tail v)
             _ -> error "bug in tc (tail)"
      , TcForall (IntSet.singleton 0) (funType [listType (TcVar 0)] (listType (TcVar 0)))))
  , ( "null"
    , (\case [VList v] -> VBool (null v)
             _ -> error "bug in tc (null)"
      , TcForall (IntSet.singleton 0) (funType [listType (TcVar 0)] boolType)))
  , ( "reverse"
    , (\case [VList v] -> VList (reverse v)
             _ -> error "bug in tc (reverse)"
      , TcForall (IntSet.singleton 0) (funType [listType (TcVar 0)] (listType (TcVar 0)))))
  , ( "trace"
    , (\case [v] -> v
             _ -> error "bug in tc (trace)"
      , TcForall (IntSet.singleton 0) (funType [TcVar 0] (TcVar 0))))
  , ( "assertEq"
    , (\case [v1, v2] -> VBool $ v1 == v2
             _ -> error "bug in tc (assertEq)"
      , TcForall (IntSet.singleton 0) (funType [TcVar 0, TcVar 0] boolType)))
  ]
  where
    intBinop :: String -> (Int -> Int -> Int) -> ([Value] -> Value, TcForall)
    intBinop nm f =
      (\case [VInt a, VInt b] -> VInt (a `f` b)
             v -> error ("bug in tc (" ++ nm ++ "). got: " ++ show v)
      , monotype (funType [intType, intType] intType))

    compareBinop :: (Value -> Value -> Bool) -> ([Value] -> Value, TcForall)
    compareBinop f =
      (\case [lhs, rhs] -> VBool (lhs `f` rhs)
             _ -> error "bug in tc (compare binop)"
      , TcForall (IntSet.singleton 0) (funType [TcVar 0, TcVar 0] boolType))

builtinsGamma :: Map String TcForall
builtinsGamma = Map.map snd builtins
