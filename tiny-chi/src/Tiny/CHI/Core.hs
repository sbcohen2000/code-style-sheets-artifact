{-# LANGUAGE TypeFamilies #-}
module Tiny.CHI.Core where

import Tiny.CHI.Types

data CoreExp x
  = CEVar (XCEVar x) (Ident x)
  | CEInt (XCEInt x) Int
  | CEApp (XCEApp x) (CoreExp x) (CoreExp x)
  | CEAbs (XCEAbs x) (Ident x) (CoreExp x)
  | CELet (XCELet x) (CBinding x) (CoreExp x)
  | CECase (XCECase x) (CoreExp x) [CPat x]

type family XCEVar x
type family XCEInt x
type family XCEApp x
type family XCEAbs x
type family XCELet x
type family XCECase x

data CBinding x
  = CBNonRec (XCBNonRec x) (Ident x) (CoreExp x)
  | CBRec (XCBRec x) (Ident x) (CoreExp x)

type family XCBNonRec x
type family XCBRec x

data CPat x
  = CPData
    (XCPData x)
    (Ident x)   -- ^ constructor name
    [Ident x]   -- ^ fresh names
    (CoreExp x) -- ^ pattern body
  | CPLit (XCPLit x) Int (CoreExp x) -- ^ literal

type family XCPData x
type family XCPLit x
