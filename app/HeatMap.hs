module Main where

import Control.Monad
import Data.Colour
import Text.Blaze.Html.Renderer.String ( renderHtml )
import Text.Blaze.Html5 ( (!) )
import TinyCommon
import qualified Data.Colour.Names as Colors
import qualified Stylesheet as S (rule)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Debug.Trace

sExpByHeat :: (Double, Double) -> Color -> Rule
sExpByHeat (lower, upper) color =
  Rule
  [ PrimitiveSelectorRule
    { targetedStyles = [([], [ ("color", toHex color, 1) ])]
    , keepOutPaths = []
    , selector = PatternSelector (-1) (select $ \(e::Exp_ EvalPhase) -> Just [Binding e]) []
    }
  ]
  $ \case [Binding e] ->
            selectP ((\x -> x > lower && x <= upper) . pct) e
            where
              pct = ev_exp_sample_percentage . getExpAnno @EvalPhase
          _ -> False

sCase = [S.rule|
ECase @EvalPhase _ (Located _ t1, _, Located _ t2, _) ->
t1 t2 {
  font-weight: bold;
}
|]

coolToWarmGradient :: Int -> [Color]
coolToWarmGradient n = map (\t -> blend t warm cool) (linspace n)
  where
    cool = Colors.royalblue
    warm = Colors.orangered

linspace :: Int -> [Double]
linspace n = map (\i -> fromIntegral i * delta) [0..n-1]
  where
    delta = 1 / fromIntegral (n - 1)

ranges :: Int -> Double -> [(Double, Double)]
ranges n max = map (\i -> let f = fromIntegral i
                          in (f * delta, (f + 1) * delta)) [0..n-1]
  where
    delta = max / fromIntegral n

addColorBar :: [Color] -> StylishText -> StylishText
addColorBar gradient (Node path cls sty children) =
  Node path cls sty (colorBar:children)
  where
    colorBar :: StylishText
    colorBar = Node Nothing [] []
      [ HtmlLeaf $ renderHtml $ H.div $
        forM_ gradient $ \color ->
          let style = H.stringValue $ "background-color: "
                      ++ toHex color
                      ++ "; height: 20px;\
                         \width: 38px;\
                         \display: inline-block;"
          in H.span ! A.style style $ pure ()
      , TextLeaf "\n" ]

nRanges :: Int
nRanges = 16

gradient :: [Color]
gradient = coolToWarmGradient nRanges

stylesheet :: [Rule]
stylesheet = zipWith sExpByHeat (ranges nRanges 1) gradient ++ sCase

main :: IO ()
main = stylesheetMain
  $ (defaultStylish @EvalPhase stylesheet)
  { stylishTransform = addColorBar gradient }
