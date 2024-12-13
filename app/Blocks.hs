module Main where

import Common
import qualified Stylesheet as S (rule)

sBinops = [S.rule|
x@EBinop _ ->
x {
  border-width: 2;
  padding: 2;
  margin: 2;
  border-radius: 3;
  border-color: navy;
  background-color: rgba(44, 90, 160, 0.2);
}
|]

stylesheet :: [Rule]
stylesheet = sBinops

main :: IO ()
main = stylesheetMain stylesheet
