module Main where

import Common
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Stylesheet as S (rule)

data Dir = LeftToRight | RightToLeft | Don'tCare deriving Eq

opDir :: Map String Dir
opDir = Map.fromList
  [ (">>=", LeftToRight)
  , (">>>", LeftToRight)
  , (".",   RightToLeft)
  , ("==",  Don'tCare)
  , ("+",   Don'tCare)
  , (":",   Don'tCare) ]

sBinopTransitionLR = [S.rule|
x@(EBinop (_, Located _ (Op op1), Located _ (y@EBinop (_, Located _ (Op op2), _)))) if isDirUnEq op1 op2 ->
x {
  border-width: 2; padding: 2; margin: 2; border-radius: 3;
  border-color: indigo;
  background-color: lavender;
}
y {
  border-width: 2; padding: 2; margin: 2; border-radius: 3;
  border-color: orange;
  background-color: papayawhip;
}
|]
  where isDirUnEq op1 op2 =
          selectP ((== LeftToRight) . (opDir !)) op1
          && selectP ((== RightToLeft) . (opDir !)) op2

sBinopTransitionRL = [S.rule|
x@(EBinop (_, Located _ (Op op1), Located _ (y@EBinop (_, Located _ (Op op2), _)))) if isDirUnEq op1 op2 ->
x {
  border-width: 2; padding: 2; margin: 2; border-radius: 3;
  border-color: orange;
  background-color: papayawhip;
}
y {
  border-width: 2; padding: 2; margin: 2; border-radius: 3;
  border-color: indigo;
  background-color: lavender;
}
|]
  where isDirUnEq op1 op2 =
          selectP ((== RightToLeft) . (opDir !)) op1
          && selectP ((== LeftToRight) . (opDir !)) op2

sBinopLR = [S.rule|
(EBinop (_, Located _ (x@Op op), _)) if isLR op ->
x {
  font-weight: bold;
  color: indigo;
}
|]
  where isLR op = selectP ((== LeftToRight) . (opDir !)) op

sBinopRL = [S.rule|
(EBinop (_, Located _ (x@Op op), _)) if isRL op ->
x {
  font-weight: bold;
  color: orange;
}
|]
  where isRL op = selectP ((== RightToLeft) . (opDir !)) op

stylesheet :: [Rule]
stylesheet = sBinopTransitionLR
             ++ sBinopTransitionRL
             ++ sBinopLR
             ++ sBinopRL

main :: IO ()
main = stylesheetMain stylesheet
