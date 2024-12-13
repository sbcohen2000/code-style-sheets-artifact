{-# LANGUAGE OverloadedStrings #-}

module Tiny.CHI.ProjectionBoxesStylish
  ( ProjectionBoxesStylish(..)
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

newtype ProjectionBoxesStylish x =
  ProjectionBoxesStylish { unStylish :: SourceFile x }

instance Stylish (ProjectionBoxesStylish EvalPhase) where
  showStylish path (ProjectionBoxesStylish arg) =
    let (_anno_, decls_, (_, env)) = subvaluesSourceFile (path, arg) in
    evalState (emitStylish env decls_)
    (StylishCodeState
     { scs_pos = startPos
     , scs_content_after = ContentAfter
       { content_after_exp = fmap view . ev_exp_snapshot
       , content_after_do_stmt = const Nothing }
     } :: StylishCodeState EvalPhase)
    where
      view :: Snapshot -> StylishText
      view = HtmlLeaf . renderHtml . snapshotView

      snapshotView :: Snapshot -> H.Html
      snapshotView (Snapshot variables table) =
        H.div ! A.class_ "outer-box" $ do
        H.span ! A.class_ "tooltip-spacer" $ pure ()
        H.span ! A.class_ "tooltip" $ pure ()
        H.div ! A.class_ "inner-box" $
          H.table ! A.class_ "table" $ do
          H.tr $ forM_ (zip variables trueFalseStream) $ \(var, order) ->
            H.th ! A.class_ (if order then "even-col-header" else "odd-col-header") $ H.toHtml var
          forM_ table $ \row ->
            H.tr $ forM_ (zip row trueFalseStream) $ \(value, order) ->
              H.td ! A.class_ (if order then "even-col" else "odd-col") $ H.toHtml value

      trueFalseStream = cycle [True, False]

subvaluesSourceFile
  :: Phase x
  => Pathed (SourceFile x)
  -> (Pathed (XSourceFile x), Pathed [TopDecl x], Pathed SourceFileInfo)
subvaluesSourceFile x@(_, SourceFile _1 _2 _3) = subvalues3 $ konst (_1, _2, _3) x
