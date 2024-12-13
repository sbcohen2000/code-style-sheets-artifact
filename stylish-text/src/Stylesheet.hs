{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Stylesheet
  ( parseRule
  , parseStylesheet
  , rule
  ) where

import Data.Text (pack)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Megaparsec (SourcePos (..), mkPos)

import Stylesheet.Parser (parseRule, parseStylesheet)

rule :: QuasiQuoter
rule = QuasiQuoter
  { quoteExp  = parse
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }
  where notHandled things = error $
          things ++ " are not handled by rule quasiquoter"

parse :: String -> Q Exp
parse s = do
  pos <- location
  let srcPos = SourcePos
        (loc_filename pos)
        (mkPos $ fst $ loc_start pos)
        (mkPos $ snd $ loc_start pos)
  case parseRule srcPos (pack s) of
    Left err -> fail err
    Right selector -> unTypeCode [e|| selector ||]
