module Main where

import Common
import Data.Char (isUpper)
import qualified Stylesheet as S (rule)

selectConstant = [S.rule|
x@EInt _,
EChar (_, x, _),
EFloat (x, y) ->
x y {
  color: teal;
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

selectDecls = [S.rule|
(Decl (Located _ (Equation ((Left (_, xxx)), xxx, xxx)))) (x@Ident _),
(Decl (Located _ (Signature (_, xxx, xxx)))) (x@Ident _),
(ClassDecl _) (Signature (_, xxx, xxx)) (x@Ident _),
(ClassDecl _) (Equation ((Left (_, xxx)), xxx, xxx)) (x@Ident _),
(InstanceDecl _) (Equation ((Left (_, xxx)), xxx, xxx)) (x@Ident _) ->
x {
  color: mediumvioletred;
}
|]

selectConstructors = [S.rule|
v@(Ident nm) if pred nm ->
v {
  color: green;
}
|]
  where pred nm =
          selectP (isUpper . head) nm

selectTypeDecls = [S.rule|
TypeDecl (Located _ t, Located _ x, _, _, _),
NewtypeDecl (Located _ t, Located _ x, _, _, _),
DataDecl (Located _ t, Located _ x, _, _, _) ->
t {
  color: blue;
}
x {
  color: green;
}
|]

selectKeywords = [S.rule|
ImportDecl (Located _ t1, _, _, _, _),
ImportDecl (_, Just (Located _ t1), _, _, _),
ImportDecl (_, _, _, Just (Located _ t1, _), _),
ECase (Located _ t1, _, Located _ t2, _),
EIf (Located _ t1, _, Located _ t2, _, Located _ t3, _),
EDo (Located _ t1, _),
DoLet (Located _ t1, _),
ModDecl (Located _ t1, _, _, Located _ t2),
ClassDecl (Located _ t1, _, _, _, Located _ t2, _),
InstanceDecl (Located _ t1, _, _, _, Located _ t2, _),
Equation (_, _, Just (Located _ t1, _)),
InfixL (Located _ t),
InfixR (Located _ t) ->
t1 t2 t3 {
  color: blue;
}
|]

selectTypes = [S.rule|
TUnit (Located _ t1, Located _ t2),
TCon (Located _ t1),
TVar (Located _ t1),
TFun (_, Located _ t1, _) ->
t1 t2 {
  color: green;
}
|]

stylesheet :: [Rule]
stylesheet =
  selectConstant
  ++ selectDecls
  ++ selectTypeDecls
  ++ selectKeywords
  ++ selectTypes
  ++ selectString
  ++ selectConstructors

main :: IO ()
main = stylesheetMain stylesheet
