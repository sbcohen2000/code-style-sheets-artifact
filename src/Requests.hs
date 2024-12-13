{-# LANGUAGE OverloadedStrings #-}

module Requests where

import           Control.Applicative        (empty)
import           Data.Aeson                 ((.=), (.:))
import           Data.Text           hiding (empty)
import qualified Data.Aeson          as J

import qualified Layout

newtype DocumentId = DocumentId { getId::String }
  deriving (Eq, Show)

data DocumentRole = SourceFile | Stylesheet

data DocumentMetadata
  = DocumentMetadata
  { doc_id::DocumentId
  , doc_role::DocumentRole
  , doc_name::Text
  }

instance J.ToJSON DocumentRole where
  toJSON SourceFile = "SourceFile"
  toJSON Stylesheet = "Stylesheet"

instance J.FromJSON DocumentRole where
  parseJSON (J.String "SourceFile") = pure SourceFile
  parseJSON (J.String "Stylesheet") = pure Stylesheet
  parseJSON _ = empty

instance J.ToJSON DocumentMetadata where
  toJSON (DocumentMetadata id role name) =
    J.object [ "id"   .= getId id
             , "role" .= role
             , "name" .= name ]

instance J.FromJSON DocumentMetadata where
  parseJSON (J.Object v) = do
    id <- DocumentId <$> v .: "id"
    role <- v .: "role"
    name <- v .: "name"
    pure $ DocumentMetadata id role name
  parseJSON _ = empty

data ToMsg
  = Measure Layout.MeasurementSpec
  | RequestLeading
  | LayoutDone String J.Value
  | Error String
  -- Tell the client that we're serving examples.
  -- Give number of examples.
  | InfoExamples Int
  -- Tell the client that we're serving custom layouts.
  | InfoCustomLayout
  | DocumentContents DocumentId Text
  | DocumentManifest [DocumentMetadata]
  | DocumentSuccess

instance J.ToJSON ToMsg where
  toJSON (Measure spec) =
    J.object [ "op"   .= ("Measure"::String)
             , "spec" .= spec ]
  toJSON RequestLeading =
    J.object [ "op"   .= ("RequestLeading"::String) ]
  toJSON (LayoutDone description layoutData) =
    J.object [ "op"          .= ("LayoutDone"::String)
             , "description" .= description
             , "data"        .= layoutData ]
  toJSON (Error message) =
    J.object [ "op"      .= ("Error"::String)
             , "message" .= message ]
  toJSON (InfoExamples nExamples) =
    J.object [ "op"        .= ("Info"::String)
             , "type"      .= ("examples"::String)
             , "nExamples" .= nExamples ]
  toJSON InfoCustomLayout =
    J.object [ "op"   .= ("Info"::String)
             , "type" .= ("customLayout"::String) ]
  toJSON (DocumentContents id contents) =
    J.object [ "op"       .= ("DocumentContents"::String)
             , "id"       .= getId id
             , "contents" .= contents ]
  toJSON (DocumentManifest metadata) =
    J.object [ "op"       .= ("DocumentManifest"::String)
             , "manifest" .= metadata ]
  toJSON DocumentSuccess =
    J.object [ "op" .= ("DocumentSuccess"::String) ]

type Document = (DocumentMetadata, Text)

data FromMsg
  = RequestExampleLayout String
  | RequestCustomLayout
    { stylesheetSrc :: Text
    , programSrc    :: Text
    }
  | RequestInfo
  | RequestDocumentManifest
  | RequestDocumentWrite  DocumentId DocumentRole Text Text
  | RequestDocumentRead   DocumentId
  | RequestDocumentDelete DocumentId
  | RequestDocumentRename DocumentId Text

instance J.FromJSON FromMsg where
  parseJSON (J.Object v) = do
    op::String <- v .: "op"
    case op of
      "RequestExampleLayout" -> RequestExampleLayout <$> v .: "exampleIndex"
      "RequestCustomLayout" -> do
        stylesheetSrc <- v .: "stylesheetSrc"
        programSrc <- v .: "programSrc"
        pure $ RequestCustomLayout stylesheetSrc programSrc
      "RequestInfo" -> pure RequestInfo
      "RequestDocumentManifest" -> pure RequestDocumentManifest
      "RequestDocumentWrite" -> do
        id <- DocumentId <$> v .: "id"
        role <- v .: "role"
        name <- v .: "name"
        contents <- v .: "contents"
        pure $ RequestDocumentWrite id role name contents
      "RequestDocumentRead" -> RequestDocumentRead . DocumentId <$> v .: "id"
      "RequestDocumentDelete" -> RequestDocumentDelete . DocumentId <$> v .: "id"
      "RequestDocumentRename" -> do
        id <- DocumentId <$> v .: "id"
        newName <- v .: "newName"
        pure $ RequestDocumentRename id newName
      _ -> empty
  parseJSON _ = empty
