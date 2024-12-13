module Tiny.CHI.DefaultStylish
  ( DefaultStylish(..)
  ) where

import Control.Monad.State
import StylishText
import StylishText.Utils
import Tiny.CHI.Anno
import Tiny.CHI.StylishCode
import Tiny.CHI.Types

newtype DefaultStylish x =
  DefaultStylish { unStylish :: SourceFile x }

instance Phase x => Stylish (DefaultStylish x) where
  showStylish path (DefaultStylish arg) =
    let (_anno_, decls_, (_, env)) = subvaluesSourceFile (path, arg) in
    evalState (emitStylish env decls_)
    (StylishCodeState
     { scs_pos = startPos
     , scs_content_after =
       ContentAfter
       { content_after_exp = const Nothing
       , content_after_do_stmt = const Nothing }
     } :: StylishCodeState x)

subvaluesSourceFile
  :: Phase x
  => Pathed (SourceFile x)
  -> (Pathed (XSourceFile x), Pathed [TopDecl x], Pathed SourceFileInfo)
subvaluesSourceFile x@(_, SourceFile _1 _2 _3) = subvalues3 $ konst (_1, _2, _3) x
