{-# LANGUAGE TypeFamilies #-}

module Tiny.CHI.Core.Anno where

import Tiny.CHI.Anno
import Tiny.CHI.Core

type instance XCEVar RenamePhase = TypeTwinRef
type instance XCEInt RenamePhase = TypeTwinRef
type instance XCEApp RenamePhase = TypeTwinRef
type instance XCEAbs RenamePhase = TypeTwinRef
type instance XCELet RenamePhase = TypeTwinRef
type instance XCECase RenamePhase = TypeTwinRef

type instance XCBNonRec RenamePhase = TypeTwinRef
type instance XCBRec RenamePhase = TypeTwinRef

type instance XCPData RenamePhase = TypeTwinRef
type instance XCPLit RenamePhase = TypeTwinRef
