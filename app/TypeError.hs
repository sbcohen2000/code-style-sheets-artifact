module Main where

import TinyCommon
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Map ( Map )
import Server
import qualified Data.Map as Map
import qualified Stylesheet as S (rule)
import Data.Colour.SRGB

type ColorName = String

data ColorRecord
  = ColorRecord
  { outlineRGB :: Color
  , fillRGB :: Color
  , colorDescription :: String
  }

niceColors :: [ColorRecord]
niceColors =
  [ ColorRecord
    { outlineRGB = sRGB24read "#502D16"
    , fillRGB = sRGB24read "#FFE6D5"
    , colorDescription = "Brown"
    }
  , ColorRecord
    { outlineRGB = sRGB24read "#2C5AA0"
    , fillRGB = sRGB24read "#D7E3F4"
    , colorDescription = "Blue"
    }
  , ColorRecord
    { outlineRGB = sRGB24read "#E5FF80"
    , fillRGB = sRGB24read "#F5FFCF"
    , colorDescription = "Green"
    }
  , ColorRecord
    { outlineRGB = sRGB24read "#F05735"
    , fillRGB = sRGB24read "#F797A7"
    , colorDescription = "Red"
    }
  ]

sExpById :: TraceId -> ColorRecord -> Rule
sExpById target color =
  Rule
  [ PrimitiveSelectorRule
    { targetedStyles =
        [([], [ ("border-color", toHex (outlineRGB color), 1)
              , ("background-color", toHex (fillRGB color), 1)
              , ("border-width", "2", 1)
              , ("padding", "2", 0)
              , ("margin", "2", 0)
              , ("border-radius", "3", 0)
              ])]
    , keepOutPaths = []
    , selector = PatternSelector (-1)
      (select $ \(e::Exp_ TypecheckPhase) -> Just [Binding e]) []
    }
  ]
  $ \case [Binding e] -> idOfExp e == target
          _ -> False

sDeclById :: TraceId -> ColorRecord -> Rule
sDeclById target color =
  Rule
  [ PrimitiveSelectorRule
    { targetedStyles =
        [([], [ ("border-color", toHex (outlineRGB color), 1)
              , ("background-color", toHex (fillRGB color), 1)
              , ("border-width", "2", 1)
              , ("padding", "2", 0)
              , ("margin", "2", 0)
              , ("border-radius", "3", 0)
              ])]
    , keepOutPaths = []
    , selector = PatternSelector (-1)
      (select $ \(d::Decl_ TypecheckPhase) -> Just [Binding d]) []
    }
  ]
  $ \case [Binding d] -> idOfDecl d == target
          _ -> False

sIf = [S.rule|
EIf @TypecheckPhase _ (Located _ t1, _, Located _ t2, _, Located _ t3, _) ->
t1 t2 t3 {
  font-weight: bold;
}
|]

sExpWithTyp :: TraceId -> String -> ColorRecord -> Rule
sExpWithTyp target typ color =
  Rule
  [ PrimitiveSelectorRule
    { targetedStyles = []
    , keepOutPaths = []
    , selector = PatternSelector (-1) (select $ \(e1::Exp_ TypecheckPhase) -> Just [Binding e1]) []
    }
  , PrimitiveSelectorRule
    { targetedStyles =
        [([], [ ("background-color", toHex (fillRGB color), 0)
              , ("border-color", toHex (outlineRGB color), 0)
              , ("border-width", "2", 0)
              , ("padding", "2", 0)
              , ("margin", "2", 0)
              , ("border-radius", "3", 0) ])]
    , keepOutPaths = []
    , selector = PatternSelector (-1) (select $ \(e2::Exp_ TypecheckPhase) -> Just [Binding e2]) []
    }
  ]
  $ \case [Binding e1, Binding e2] ->
            idOfExp e1 == target && selectP ((==typ) . tc_typ_string . getExpAnno @TypecheckPhase) e2
          _ -> False

sDeclWithTyp :: TraceId -> String -> ColorRecord -> Rule
sDeclWithTyp target typ color =
  Rule
  [ PrimitiveSelectorRule
    { targetedStyles = []
    , keepOutPaths = []
    , selector = PatternSelector (-1) (select $ \(d::Decl_ TypecheckPhase) -> Just [Binding d]) []
    }
  , PrimitiveSelectorRule
    { targetedStyles =
        [([], [ ("background-color", toHex (fillRGB color), 0)
              , ("border-color", toHex (outlineRGB color), 0)
              , ("border-width", "2", 0)
              , ("padding", "2", 0)
              , ("margin", "2", 0)
              , ("border-radius", "3", 0) ])]
    , keepOutPaths = []
    , selector = PatternSelector (-1) (select $ \(e::Exp_ TypecheckPhase) -> Just [Binding e]) []
    }
  ]
  $ \case [Binding d, Binding e] ->
            idOfDecl d == target && selectP ((==typ) . tc_typ_string . getExpAnno @TypecheckPhase) e
          _ -> False

data ColoredTcError
  = ColoredUnificationError TraceId ColorRecord (ColorRecord, TcTyp) (ColorRecord, TcTyp)
  | ColoredUnboundVariableError TraceId ColorRecord String Range

nextColor :: String -> State (Int, Map String ColorRecord) ColorRecord
nextColor typeString = do
  table <- gets snd
  case Map.lookup typeString table of
    Just colorName -> pure colorName
    Nothing -> do
      let
        nKeys = length niceColors

      index <- gets fst
      let color = niceColors !! index
      modify (\(idx, map) ->
                ((idx + 1) `mod` nKeys, Map.insert typeString color map))
      pure color

assignColorsToError
  :: TypecheckError
  -> State (Int, Map String ColorRecord) ColoredTcError
assignColorsToError (UnificationError tid ta tb) = do
  let
    taString = pprintType ta defaultNameMap
    tbString = pprintType tb defaultNameMap
  c  <- nextColor (show tid)
  ca <- nextColor taString
  cb <- nextColor tbString
  pure $ ColoredUnificationError tid c (ca, ta) (cb, tb)
assignColorsToError (UnboundVariableError tid nm range) = do
  c <- nextColor ""
  pure $ ColoredUnboundVariableError tid c nm range

pprintError :: ColoredTcError -> IO ()
pprintError (ColoredUnificationError _ c (ca, ta) (cb, tb)) =
  putStrLn $
  "in "
  ++ withANSIColor cRGB cDescription
  ++ " expression, could not unify "
  ++ withANSIColor caRGB taString
  ++ " and "
  ++ withANSIColor cbRGB tbString
  where
    taString = pprintType ta defaultNameMap
    tbString = pprintType tb defaultNameMap

    cDescription = colorDescription c
    cRGB = outlineRGB c
    caRGB = outlineRGB ca
    cbRGB = outlineRGB cb
pprintError (ColoredUnboundVariableError _ c nm range) =
  putStrLn $
  withANSIColor cRGB nm ++ " is unbound at " ++ show range
  where
    cRGB = outlineRGB c

pyretErrors
  :: SourceFile ParsePhase
  -> MsgHandler (RenderableStylishText MsgHandler)
pyretErrors =
  uptoTypechecking $ \(errors, typechecked) -> do
  let
    coloredErrors =
      evalState (mapM assignColorsToError errors) (0, Map.empty)

    stylesheet :: [Rule]
    stylesheet =
      concatMap
      (\case
          (ColoredUnificationError tid c (ca, ta) (cb, tb)) ->
            let
              taString = pprintType ta defaultNameMap
              tbString = pprintType tb defaultNameMap
            in [ sExpById tid c
               , sDeclById tid c
               , sExpWithTyp tid taString ca
               , sDeclWithTyp tid taString ca
               , sExpWithTyp tid tbString cb
               , sDeclWithTyp tid taString cb ]
          (ColoredUnboundVariableError tid c _ _) ->
            [sExpById tid c]) coloredErrors

  liftIO $ forM_ coloredErrors pprintError

  let stylishText =
        applyStyles typechecked (stylesheet ++ sIf)
        $ showStylish []
        $ DefaultStylish typechecked

  pure $ RenderableStylishText stylishText measureString

main :: IO ()
main = stylesheetMainImpl pyretErrors
