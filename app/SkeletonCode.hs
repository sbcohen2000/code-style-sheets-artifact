module Main where

import Common
import qualified Stylesheet as S (rule)

selectMain = [S.rule|
x@Equation (Left (Located _ (PVar (Located _ (y@Ident "main"))), _), _, _) ->
x y {
  color: gray;
}
|]

selectMod = [S.rule|
x@ModDecl _ ->
x {
  color: gray;
}
|]

selectTVar = [S.rule|
TVar (Located _ t1) ->
t1 {
  color: green;
}
|]

selectDecls = [S.rule|
(Decl (Located _ (Signature (_, xxx, xxx)))) (x@Ident _),
(Decl (Located _ (Equation ((Left (_, xxx)), xxx, xxx)))) (x@Ident _) ->
x {
  color: mediumvioletred;
  font-size: 18px;
}
|]

selectComment = [S.rule|
x@.comment ->
x {
  font-family: "Noto Serif", serif;
}
|]

stylesheet :: [Rule]
stylesheet =
  selectDecls
  ++ selectMain
  ++ selectMod
  ++ selectTVar
  ++ selectComment

main :: IO ()
main = stylesheetMain stylesheet
