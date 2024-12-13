{-# LANGUAGE OverloadedStrings #-}

module Tiny.CHI.ClassicProjectionBoxesStylish
  ( ClassicProjectionBoxesStylish(..)
  ) where

import Control.Monad
import Control.Monad.State
import StylishText
import StylishText.Utils
import Text.Blaze.Html.Renderer.String ( renderHtml )
import Text.Blaze.Html5 ( (!) )
import Tiny.CHI.Anno
import Tiny.CHI.StylishCode
import Tiny.CHI.Types
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

newtype ClassicProjectionBoxesStylish x =
  ClassicProjectionBoxesStylish { unStylish :: SourceFile x }

instance Stylish (ClassicProjectionBoxesStylish EvalPhase) where
  showStylish path (ClassicProjectionBoxesStylish arg) =
    let (_anno_, decls_, (_, env)) = subvaluesSourceFile (path, arg) in
    evalState (emitStylish env decls_)
    (StylishCodeState
     { scs_pos = startPos
     , scs_content_after = ContentAfter
       { content_after_exp = const Nothing
       , content_after_do_stmt = fmap view . ev_do_snapshot }
     } :: StylishCodeState EvalPhase)
    where
      view :: Snapshot -> StylishText
      view = HtmlLeaf . renderHtml . snapshotView

      outerBox = "position: relative;\
                 \display: flex;\
                 \margin: 3px;\
                 \flex-direction: row;"
      innerBox = "max-height: 4em;\
                 \overflow: auto;\
                 \border: 2px solid gray;\
                 \border-radius: 3px;"
      lineStyle = "border-width: 2px;\
                  \margin-top: 0.5em;\
                  \width: 40px;\
                  \border-top-style: solid;\
                  \border-color: gray;"
      tableStyle = "border-collapse: collapse;\
                   \border-spacing: 0;"

      evenCol = "background-color: #D7E3F4"
      oddCol = ""
      evenHeaderCol = "position: sticky;\
                      \top: 0;\
                      \background-color: #D7E3F4;\
                      \box-shadow: 0 2px 0 gray;"
      oddHeaderCol = "position: sticky;\
                     \top: 0;\
                     \background-color: white;\
                     \box-shadow: 0 2px 0 gray;"

      snapshotView :: Snapshot -> H.Html
      snapshotView (Snapshot variables table) =
        H.div ! A.style outerBox $ do
        H.span ! A.style lineStyle $ pure ()
        H.div ! A.style innerBox $
          H.table ! A.style tableStyle $ do
          H.tr $ forM_ (zip variables trueFalseStream) $ \(var, order) ->
            H.th ! A.style (if order then evenHeaderCol else oddHeaderCol) $ H.toHtml var
          forM_ table $ \row ->
            H.tr $ forM_ (zip row trueFalseStream) $ \(value, order) ->
              H.td ! A.style (if order then evenCol else oddCol) $ H.toHtml value

      trueFalseStream = cycle [True, False]

subvaluesSourceFile
  :: Phase x
  => Pathed (SourceFile x)
  -> (Pathed (XSourceFile x), Pathed [TopDecl x], Pathed SourceFileInfo)
subvaluesSourceFile x@(_, SourceFile _1 _2 _3) = subvalues3 $ konst (_1, _2, _3) x
