module Main where

import TinyCommon
import qualified Stylesheet as S (rule)

nColors :: Int
nColors = 6

bindUse1 = [S.rule|
x@(Ident @RenamePhase (IdentBinding uniq) _) if pred uniq,
x@(Ident @RenamePhase (IdentUsage _ binder) _) if pred binder ->
x {
  color: brown;
}
|]
  where pred bind =
          selectP (\(TraceId id) -> id `mod` nColors == 0) bind

bindUse2 = [S.rule|
x@(Ident @RenamePhase (IdentBinding uniq) _) if pred uniq,
x@(Ident @RenamePhase (IdentUsage _ binder) _) if pred binder ->
x {
  color: darkcyan;
}
|]
  where pred bind =
          selectP (\(TraceId id) -> id `mod` nColors == 1) bind

bindUse3 = [S.rule|
x@(Ident @RenamePhase (IdentBinding uniq) _) if pred uniq,
x@(Ident @RenamePhase (IdentUsage _ binder) _) if pred binder ->
x {
  color: darkolivegreen;
}
|]
  where pred bind =
          selectP (\(TraceId id) -> id `mod` nColors == 2) bind

bindUse4 = [S.rule|
x@(Ident @RenamePhase (IdentBinding uniq) _) if pred uniq,
x@(Ident @RenamePhase (IdentUsage _ binder) _) if pred binder ->
x {
  color: purple;
}
|]
  where pred bind =
          selectP (\(TraceId id) -> id `mod` nColors == 3) bind

bindUse5 = [S.rule|
x@(Ident @RenamePhase (IdentBinding uniq) _) if pred uniq,
x@(Ident @RenamePhase (IdentUsage _ binder) _) if pred binder ->
x {
  color: orange;
}
|]
  where pred bind =
          selectP (\(TraceId id) -> id `mod` nColors == 4) bind

bindUse6 = [S.rule|
x@(Ident @RenamePhase (IdentBinding uniq) _) if pred uniq,
x@(Ident @RenamePhase (IdentUsage _ binder) _) if pred binder ->
x {
  color: red;
}
|]
  where pred bind =
          selectP (\(TraceId id) -> id `mod` nColors == 5) bind

stylesheet :: [Rule]
stylesheet = bindUse1
             ++ bindUse2
             ++ bindUse3
             ++ bindUse4
             ++ bindUse5
             ++ bindUse6

main :: IO ()
main = stylesheetMain $ defaultStylish @RenamePhase stylesheet
