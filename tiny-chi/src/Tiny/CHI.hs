module Tiny.CHI
  ( module Tiny.CHI.Parser
  , module Tiny.CHI.Value
  , module Tiny.CHI.Typecheck
  , Tiny.CHI.Rename.renameExp
  , Tiny.CHI.Rename.rename
  , Tiny.CHI.evalExp
  , Tiny.CHI.Eval.eval
  , Tiny.CHI.Eval.RuntimeError
  ) where

import Control.Monad.Except
import Control.Monad.State
import Tiny.CHI.Anno
import Tiny.CHI.Eval
import Tiny.CHI.Parser
import Tiny.CHI.Typecheck
import Tiny.CHI.Types
import Tiny.CHI.Value
import qualified Data.Map as Map
import qualified Tiny.CHI.Rename ( renameExp, rename )

evalExp :: Exp TypecheckPhase -> IO (Either RuntimeError Value)
evalExp e = do
  res <- evalStateT (runExceptT (Tiny.CHI.Eval.evalExp Map.empty e)) initialEvalState
  pure $ case res of
    Left err -> Left err
    Right v -> Right v
