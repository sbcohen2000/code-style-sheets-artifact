{-# LANGUAGE LambdaCase #-}

module Tiny.CHI.Rename
  ( rename
  , renameExp ) where

import Control.Applicative
import Control.Monad.State
import Data.Generics
import Data.Map ( Map )
import Data.Maybe
import Tiny.CHI.Anno
import Tiny.CHI.Builtins
import Tiny.CHI.Types
import Tiny.CHI.Util
import qualified Data.Map as Map

data RenameState = RenameState
  { next_fresh_trace_id :: Int -- for expressions and declarations
  , next_fresh_ident_id :: Int  -- for identifiers
  }

initialState :: RenameState
initialState = RenameState 0 0

type Rename = State RenameState

freshTraceId :: Rename TraceId
freshTraceId = do
  id <- gets next_fresh_trace_id
  modify (\s -> s { next_fresh_trace_id = next_fresh_trace_id s + 1})
  pure (TraceId id)

freshIdentId :: Rename TraceId
freshIdentId = do
  id <- gets next_fresh_ident_id
  modify (\s -> s { next_fresh_ident_id = next_fresh_ident_id s + 1})
  pure (TraceId id)

transform :: TransformOps ParsePhase RenamePhase Rename
transform =
  TransformOps
  { t_ident = const (fmap IdentUnbound freshIdentId)
  , t_op = doNothing
  , t_exp = const freshTraceId
  , t_xexp = doNothing
  , t_pat = doNothing
  , t_xpat = doNothing
  , t_decl = const freshTraceId
  , t_xdecl = doNothing
  , t_typ = doNothing
  , t_xtyp = doNothing
  , t_knd = doNothing
  , t_xknd = doNothing
  , t_do = const freshTraceId
  , t_xdo = doNothing
  , t_topDecl = doNothing
  , t_xtopDecl = doNothing
  , t_sourceFile = doNothing
  }
  where doNothing = const (pure ())

rename :: SourceFile ParsePhase -> SourceFile RenamePhase
rename sourceFile = resolveBindingsAndUsages
                    $ evalState (transformAnno transform sourceFile)
                    initialState

renameExp :: Exp ParsePhase -> Exp RenamePhase
renameExp e = resolveBindingsAndUsages
              $ evalState (transformExp transform e)
              initialState

type ResolveState = Map String TraceId
type Resolve = State ResolveState

resolveBindingsAndUsages :: forall a. Data a => a -> a
resolveBindingsAndUsages a = evalState (everywhere'M action a) initialState
  where
    action :: GenericM Resolve
    action = mkM (\case (PVar @RenamePhase pAnno ident) ->
                          PVar pAnno <$> handleBindingSite ident
                        a -> pure a)
             `extM` (\case (EVar @RenamePhase eAnno ident) ->
                             EVar eAnno <$> handleUsageSite ident
                           a -> pure a)

    handleBindingSite :: Ident RenamePhase -> Resolve (Ident RenamePhase)
    handleBindingSite (Located loc (Ident anno id)) = do
      let uniq = id_uniq anno
      modify (Map.insert id uniq)
      pure $ Located loc (Ident (IdentBinding uniq) id)

    handleUsageSite :: Ident RenamePhase -> Resolve (Ident RenamePhase)
    handleUsageSite (Located loc (Ident anno id)) = do
      let uniq = id_uniq anno
      maybeLocalUsage <- gets (fmap (IdentUsage uniq) . Map.lookup id)
      let maybeGlobalUsage =
            if Map.member id builtins
            then Just (IdentGlobalUsage uniq)
            else Nothing
          anno' =
            fromMaybe (IdentUnbound uniq) (maybeLocalUsage <|> maybeGlobalUsage)
      pure $ Located loc (Ident anno' id)

    initialState :: ResolveState
    initialState = Map.empty

everywhere'M :: forall m. Monad m => GenericM m -> GenericM m
everywhere'M f = go
  where
    go :: GenericM m
    go x = do
      x' <- f x
      gmapM go x'
