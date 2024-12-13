{-# LANGUAGE LambdaCase #-}
module TinyCommon
  ( measureString
  , stylesheetMain
  , stylesheetMainImpl
  , projectionBoxesStylish
  , defaultStylish
  , uptoTypechecking
  , UserError(..)
  , module Tiny.CHI
  , module Tiny.CHI.Anno
  , module Tiny.CHI.DefaultStylish
  , module Tiny.CHI.Types
  , module StylishText
  , module StylishText.Renderable
  , Color
  , withANSIColor
  , toHex
  , Settings(..)
  , addCSS
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Colour
import Data.Colour.SRGB
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.Text ( Text )
import Requests
import Server
import StylishText
import StylishText.Renderable
import System.Environment
import Tiny.CHI
import Tiny.CHI.Anno
import Tiny.CHI.DefaultStylish
import Tiny.CHI.ProjectionBoxesStylish
import Tiny.CHI.Types
import qualified Data.Text.IO as Text
import qualified Layout
import qualified Layout.Geometry as G

type BasicExampleServer a = a -> MsgHandler (RenderableStylishText MsgHandler)

data UserError
  = CLIError { cli_err::String }
  | ParseError { parse_err::String }
  | TypecheckError { type_err::String }
  | RuntimeError { runtime_err::String }

instance Show UserError where
  show (CLIError cli_error) = show cli_error
  show (ParseError parse_err) = show parse_err
  show (TypecheckError type_err) = show type_err
  show (RuntimeError runtime_err) = show runtime_err

instance Exception UserError where

uptoRenaming
  :: BasicExampleServer (SourceFile RenamePhase)
  -> BasicExampleServer (SourceFile ParsePhase)
uptoRenaming rnm file = do
  when (anyErrors file) $ liftIO $ throwIO (ParseError "parse error")
  let renamed = rename file
  rnm renamed

uptoTypechecking
  :: BasicExampleServer ([TypecheckError], SourceFile TypecheckPhase)
  -> BasicExampleServer (SourceFile ParsePhase)
uptoTypechecking tc =
  uptoRenaming (\renamed ->
                  let (errors, typechecked) = typecheck renamed
                  in tc (errors, typechecked))

uptoEvaluation
  :: BasicExampleServer (Maybe RuntimeError, SourceFile EvalPhase)
  -> BasicExampleServer (SourceFile ParsePhase)
uptoEvaluation ev =
  uptoTypechecking (\(_, typechecked) -> do
                       (maybeRuntimeError, eval'd) <- liftIO $ eval typechecked
                       ev (maybeRuntimeError, eval'd))

talk
  :: BasicExampleServer (SourceFile ParsePhase)
  -> String
  -> Text
  -> ()
  -> FromMsg
  -> MsgHandler (IterationDecision, ())
talk f filename program _ msg =
  case msg of
    RequestExampleLayout _ -> do
      file <- liftIO $ parseSourceFile program
      renderable <- f file
      Just leading <- rpc RequestLeading
      layoutData <- Layout.layout leading renderable
      broadcast (LayoutDone filename layoutData)
      pure (Break, ())
    RequestInfo -> do
      broadcast (InfoExamples 1)
      pure (Break, ())
    _ -> error "Recieved a message which I don't know how to handle!"

anyErrors :: GenericQ Bool
anyErrors = gExists $
  False
  `mkQ`  (\case { XExp     @ParsePhase (ErrorExp     _) -> True; _ -> False })
  `extQ` (\case { XPat     @ParsePhase (ErrorPat     _) -> True; _ -> False })
  `extQ` (\case { XDecl    @ParsePhase (ErrorDecl    _) -> True; _ -> False })
  `extQ` (\case { XTyp     @ParsePhase (ErrorTyp     _) -> True; _ -> False })
  `extQ` (\case { XKnd     @ParsePhase (ErrorKnd     _) -> True; _ -> False })
  `extQ` (\case { XDo      @ParsePhase (ErrorDo      _) -> True; _ -> False })
  `extQ` (\case { XTopDecl @ParsePhase (ErrorTopDecl _) -> True; _ -> False })

measureString :: Layout.MeasurementSpec -> MsgHandler (G.Size Double)
measureString s = do
  Just rect <- rpc (Measure s)
    ::MsgHandler (Maybe (G.Rect Double))
  pure (G.size rect)

gExists :: GenericQ Bool -> GenericQ Bool
gExists = everything (||)

type Color = Colour Double

toHex :: Color -> String
toHex color =
  '#':(toHexDigit (fromIntegral r)
       ++ toHexDigit (fromIntegral g)
       ++ toHexDigit (fromIntegral b))
  where
    toHexDigit :: Int -> String
    toHexDigit n =
      [digits !! (n `div` 16), digits !! (n `mod` 16)]

    digits = ['0'..'9'] ++ ['A'..'F']

    RGB r g b = toSRGB24 color

withANSIColor :: Color -> String -> String
withANSIColor color msg =
  "\ESC[38;5;" ++ show (toANSI256 color) ++ "m" ++ msg ++ "\ESC[0m"
  where
    -- https://stackoverflow.com/a/26665998
    toANSI256 :: Color -> Int
    toANSI256 color
      | r == g && g == b =
        if r < 8 then 16
        else if r > 248 then 231
             else round (((rFrac - 8) / 247) * 24) + 232
      | otherwise =
        let
          r' = 36 * round (rFrac / 255 * 5)
          g' = 6 * round (gFrac / 255 * 5)
          b' = round (bFrac / 255 * 5)
        in 16 + r' + g' + b'
      where
        rFrac = fromIntegral r :: Float
        gFrac = fromIntegral g :: Float
        bFrac = fromIntegral b :: Float

        RGB r g b = toSRGB24 color

data Settings x s =
  Settings
  { theStylesheet :: Stylesheet
  , stylishInstance :: SourceFile x -> s
  , stylishTransform :: StylishText -> StylishText
  }

defaultStylish :: Stylesheet -> Settings x (DefaultStylish x)
defaultStylish stylesheet = Settings
  { theStylesheet = stylesheet
  , stylishInstance = DefaultStylish
  , stylishTransform = id
  }

projectionBoxesStylish :: Stylesheet -> Settings x (ProjectionBoxesStylish x)
projectionBoxesStylish stylesheet = Settings
  { theStylesheet = stylesheet
  , stylishInstance = ProjectionBoxesStylish
  , stylishTransform = id
  }

class StylesheetMain a where
  stylesheetMain :: a -> IO ()

stylesheetMainImpl :: BasicExampleServer (SourceFile ParsePhase) -> IO ()
stylesheetMainImpl srv = do
    programName <- head <$> getArgs
    program <- Text.readFile programName
    putStrLn "listening on port 1234!"
    serverMain () (talk srv programName program)

instance Stylish s => StylesheetMain (Settings EvalPhase s) where
  stylesheetMain settings = stylesheetMainImpl srv

    where
      srv :: BasicExampleServer (SourceFile ParsePhase)
      srv = uptoEvaluation $ \(maybeRuntimeError, eval'd) -> do

        case maybeRuntimeError of
          Just err -> liftIO $ throwIO (RuntimeError (show err))
          Nothing -> pure ()

        let
          stylishText =
            stylishTransform settings
            $ applyStyles eval'd (theStylesheet settings)
            $ showStylish []
            $ stylishInstance settings eval'd

        pure $ RenderableStylishText stylishText measureString

instance Stylish s => StylesheetMain (Settings RenamePhase s) where
  stylesheetMain settings = stylesheetMainImpl srv

    where
      srv :: BasicExampleServer (SourceFile ParsePhase)
      srv = uptoRenaming $ \renamed -> do
        let
          stylishText =
            stylishTransform settings
            $ applyStyles renamed (theStylesheet settings)
            $ showStylish []
            $ stylishInstance settings renamed

        pure $ RenderableStylishText stylishText measureString

addCSS :: String -> StylishText -> StylishText
addCSS css (Node path cls sty children) =
  Node path cls sty (styles:children)
  where
    styles :: StylishText
    styles = Node Nothing [] []
      [ HtmlLeaf $ "<style>" ++ css ++ "</style>" ]
addCSS _ _ = error "Can't add <style> to a leaf node"
