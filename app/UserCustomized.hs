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

sBinopLR = [S.rule|
(EBinop (_, Located _ (x@Op op), _)) if isLR op ->
x {
  border-width: 2; padding: 2; margin: 2; border-radius: 3;
  font-weight: bold;
  border-color: indigo;
  background-color: lavender;
}
|]
  where isLR op = selectP ((== LeftToRight) . (opDir !)) op

sBinopRL = [S.rule|
(EBinop (_, Located _ (x@Op op), _)) if isRL op ->
x {
  border-width: 2; padding: 2; margin: 2; border-radius: 3;
  font-weight: bold;
  border-color: orange;
  background-color: papayawhip;
}
|]
  where isRL op = selectP ((== RightToLeft) . (opDir !)) op

selectDecl = [S.rule|
(Decl (Located _ (Equation ((Left (_, xxx)), xxx, xxx)))) (x@Ident _) ->
x {
  color: mediumvioletred;
}
|]

selectString = [S.rule|
(EString (Located _ t1, _, Located _ t2)) x@.char ->
x {
  color: teal;
}
t1 t2 {
  color: blue;
}
|]

stylesheet :: [Rule]
stylesheet = sBinopLR
             ++ sBinopRL
             ++ selectDecl
             ++ selectString

main :: IO ()
main = stylesheetMain stylesheet
