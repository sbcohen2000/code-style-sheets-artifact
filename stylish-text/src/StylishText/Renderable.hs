module StylishText.Renderable
  ( RenderableStylishText(..)
  ) where

import Control.Monad.State
import Data.List
import Data.Maybe
import Text.Read ( readMaybe )
import qualified Layout.Geometry as G

import Layout
import StylishText

data RenderableStylishText m
  = RenderableStylishText
    { rs_root::StylishText
    , rs_measure::MeasurementSpec -> m (G.Size Double)
    }

instance MonadIO m => Renderable (RenderableStylishText m) m where
  style (RenderableStylishText (TextLeaf _) _) = error "style method applied to TextLeaf!"
  style (RenderableStylishText (HtmlLeaf _) _) = error "style method applied to HtmlLeaf!"
  style (RenderableStylishText (Node p cls sty _) _) =
    pure ComputedStyle
    { csPadding         = fromMaybe 0 (readMaybe =<< padding)
    , csMargin          = fromMaybe 0 (readMaybe =<< margin)
    , csBorderWidth     = fromMaybe 0 (readMaybe =<< borderWidth)
    , csBorderColor     = fromMaybe "black" borderColor
    , csBackgroundColor = fromMaybe "transparent" backgroundColor
    , csBorderRadius    = fromMaybe 0 (readMaybe =<< borderRadius)
    , csOtherStyles     = styles'
    , csInspectorInfo   = inspectorInfo
    }
    where inspectorInfo =
            InspectorInfo
            { iiPath = p
            , iiCls = cls
            }

          (( padding, margin, borderWidth, borderColor, backgroundColor, borderRadius), styles') =
            flip runState styles $ do
            padding         <- extract "padding"
            margin          <- extract "margin"
            borderWidth     <- extract "border-width"
            borderColor     <- extract "border-color"
            backgroundColor <- extract "background-color"
            borderRadius    <- extract "border-radius"
            pure (padding, margin, borderWidth, borderColor, backgroundColor, borderRadius)

          styles = map stripPrecedence sty

          stripPrecedence (k, v, _) = (k, v)

          -- extract a key from styles and return
          -- the value along with the new styles
          extract :: String -> State Layout.Styles (Maybe String)
          extract key = do
            val <- gets $ fmap snd . find ((key ==) . fst)
            modify $ filter ((key /=) . fst)
            pure val

  children (RenderableStylishText (TextLeaf _) _) = error "children method applied to TextLeaf!"
  children (RenderableStylishText (HtmlLeaf _) _) = error "children method applied to HtmlLeaf!"
  children (RenderableStylishText (Node _ _ sty children) measure) =
    concat <$> mapM go children
    where go n@(Node {}) = pure [Right $ RenderableStylishText n measure]
          go (TextLeaf s) = mapM stringToFragment (groupBy newlines s)
          go (HtmlLeaf s) = singleton . Left . FHtml s
            <$> measure (MeasureHtml s styWithoutPrec)

          stringToFragment "\n" =
            pure $ Left FNewline
          stringToFragment s =
            Left . FText s
            <$> measure (MeasureText s styWithoutPrec)

          styWithoutPrec :: Layout.Styles
          styWithoutPrec = map (\(p, v, _) -> (p, v)) sty

          newlines '\n' _ = False
          newlines _ '\n' = False
          newlines _    _ = True
