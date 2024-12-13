{-# LANGUAGE TypeFamilies #-}

module Tiny.CHI.Anno where

import Data.Data
import Data.Generics.Aliases
import Data.Map
import Tiny.CHI.Typecheck.Types
import Tiny.CHI.Types

class Phase a where

--------------------------------------------------------------------------------
-- Parse Phase

data ParsePhase
  deriving (Data, Show)

instance Phase ParsePhase where

type instance XIdent ParsePhase = ()

type instance XOp ParsePhase = ()

type instance XExp ParsePhase = ()
newtype ErrorExp = ErrorExp String
  deriving (Data, Show)
type instance XXExp ParsePhase = ErrorExp

type instance XPat ParsePhase = ()
newtype ErrorPat = ErrorPat String
  deriving (Data, Show)
type instance XXPat ParsePhase = ErrorPat

type instance XDecl ParsePhase = ()
newtype ErrorDecl = ErrorDecl String
  deriving (Data, Show)
type instance XXDecl ParsePhase = ErrorDecl

type instance XTyp ParsePhase = ()
newtype ErrorTyp = ErrorTyp String
  deriving (Data, Show)
type instance XXTyp ParsePhase = ErrorTyp

type instance XKnd ParsePhase   = ()
newtype ErrorKnd = ErrorKnd String
  deriving (Data, Show)
type instance XXKnd ParsePhase = ErrorKnd

type instance XDo ParsePhase  = ()
newtype ErrorDo = ErrorDo String
  deriving (Data, Show)
type instance XXDo ParsePhase = ErrorDo

type instance XTopDecl ParsePhase = ()
type instance XDecl ParsePhase = ()
newtype ErrorTopDecl = ErrorTopDecl String
  deriving (Data, Show)
type instance XXTopDecl ParsePhase = ErrorTopDecl

type instance XSourceFile ParsePhase = ()

--------------------------------------------------------------------------------
-- Rename Phase

data RenamePhase
  deriving (Data, Show)

instance Phase RenamePhase where

data IdentInfo
  = IdentBinding     { id_uniq :: TraceId }
  | IdentUsage       { id_uniq :: TraceId, id_binder :: TraceId }
  | IdentGlobalUsage { id_uniq :: TraceId }
  | IdentUnbound     { id_uniq :: TraceId }
  deriving (Data, Show)

type instance XIdent RenamePhase = IdentInfo

type instance XOp RenamePhase = ()

-- TODO: Once we implement core, we'll need some way to
--       figure out what the types of the sugared expressions
--       were.
--
-- | TypeTwinRef links the type of an AST expression to
--   its corresponding Core expression. The idea is that
--   if an expression in Core with id a has type t, then
--   an expression in the AST with id a must also have
--   type t.
newtype TypeTwinRef = TypeTwinRef { unref :: Int }
  deriving (Eq, Ord)

type TypeReconstructor = Map TypeTwinRef (Typ TypecheckPhase) -> Typ TypecheckPhase

-- | a unique ID for each expression for the purposes of
--   execution tracing.
newtype TraceId = TraceId Int
  deriving (Eq, Show, Data)

type instance XExp RenamePhase  = TraceId
type instance XXExp RenamePhase = ()

type instance XPat RenamePhase  = ()
type instance XXPat RenamePhase = ()

type instance XDecl RenamePhase  = TraceId
type instance XXDecl RenamePhase = ()

type instance XTyp RenamePhase   = ()
type instance XXTyp RenamePhase  = ()

type instance XKnd RenamePhase   = ()
type instance XXKnd RenamePhase  = ()

type instance XDo RenamePhase  = TraceId
type instance XXDo RenamePhase = ()

type instance XTopDecl RenamePhase  = ()
type instance XXTopDecl RenamePhase = ()

type instance XSourceFile RenamePhase = ()

--------------------------------------------------------------------------------
-- Typecheck Phase

data TypecheckPhase
  deriving (Data, Show)

instance Phase TypecheckPhase where

type instance XIdent TypecheckPhase = IdentInfo

type instance XOp TypecheckPhase = ()

data TcExpInfo
  = TcExpInfo
  { tc_traceId :: TraceId
  -- It's useful to have a string representation
  -- of the type here so that we can select a specific
  -- type during pattern matching.
  , tc_typ_string :: String
  , tc_typ :: TcTyp
  }
  deriving (Show, Data)

idOfExp :: GenericQ TraceId
idOfExp = gmapQi 0 (mkQ (TraceId (-1)) (\TcExpInfo { tc_traceId } -> tc_traceId))

idOfDecl :: GenericQ TraceId
idOfDecl = gmapQi 0 (mkQ (TraceId (-1)) id)

type instance XExp TypecheckPhase  = TcExpInfo
type instance XXExp TypecheckPhase = ()

type instance XPat TypecheckPhase  = ()
type instance XXPat TypecheckPhase = ()

type instance XDecl TypecheckPhase  = TraceId
type instance XXDecl TypecheckPhase = ()

type instance XTyp TypecheckPhase   = ()
type instance XXTyp TypecheckPhase  = ()

type instance XKnd TypecheckPhase   = ()
type instance XXKnd TypecheckPhase  = ()

type instance XDo TypecheckPhase  = TraceId
type instance XXDo TypecheckPhase = ()

type instance XTopDecl TypecheckPhase  = ()
type instance XXTopDecl TypecheckPhase = ()

type instance XSourceFile TypecheckPhase = ()

--------------------------------------------------------------------------------
-- Eval Phase

data EvalPhase
  deriving (Data, Show)

instance Phase EvalPhase where

type instance XIdent EvalPhase = IdentInfo

type instance XOp EvalPhase = ()

data Snapshot
  = Snapshot
  { snapshot_variables :: [String]
  , snapshot_table :: [[String]]
  }
  deriving (Show, Data)

data TestCaseStatus = Passed | Failed | NotRun
  deriving (Show, Data)

data EvalExpInfo
  = EvalExpInfo
  { ev_exp_traceId :: TraceId
  , ev_exp_sample_percentage :: Double
  , ev_exp_snapshot :: Maybe Snapshot
  , ev_exp_test_status :: TestCaseStatus
  }
  deriving (Show, Data)

data EvalDoStmtInfo
  = EvalDoStmtInfo
  { ev_do_traceId :: TraceId
  , ev_do_snapshot :: Maybe Snapshot
  }
  deriving (Show, Data)

type instance XExp EvalPhase  = EvalExpInfo
type instance XXExp EvalPhase = ()

type instance XPat EvalPhase  = ()
type instance XXPat EvalPhase = ()

type instance XDecl EvalPhase  = TraceId
type instance XXDecl EvalPhase = ()

type instance XTyp EvalPhase   = ()
type instance XXTyp EvalPhase  = ()

type instance XKnd EvalPhase   = ()
type instance XXKnd EvalPhase  = ()

type instance XDo EvalPhase  = EvalDoStmtInfo
type instance XXDo EvalPhase = ()

type instance XTopDecl EvalPhase  = ()
type instance XXTopDecl EvalPhase = ()

type instance XSourceFile EvalPhase = ()
