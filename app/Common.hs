module Common
  ( measureString
  , stylesheetMain
  , module CHI.Types
  , module StylishText
  ) where

import CHI.Types
import Control.Monad.IO.Class
import Data.Text ( Text )
import Data.Time.Clock
import Requests
import Server
import StylishText
import StylishText.Renderable
import System.Environment (getArgs)
import qualified CHI
import qualified Data.Text.IO as Text (readFile)
import qualified Layout
import qualified Layout.Geometry as G

measureString :: Layout.MeasurementSpec -> MsgHandler (G.Size Double)
measureString s = do
  Just rect <- rpc (Measure s)
    ::MsgHandler (Maybe (G.Rect Double))
  pure (G.size rect)

talk
  :: String
  -> Text
  -> Stylesheet
  -> ()
  -> FromMsg
  -> MsgHandler (IterationDecision, ())
talk filename program stylesheet _ msg = do
  case msg of
    RequestExampleLayout _ -> do
      startTime <- liftIO getCurrentTime
      file <- liftIO $ CHI.parseSourceFile program
      let styled = eval file
          renderable = RenderableStylishText styled measureString
      Just leading <- rpc RequestLeading
      layoutData <- Layout.layout leading renderable
      broadcast (LayoutDone filename layoutData)
      doneTime <- liftIO getCurrentTime
      let deltaTime = nominalDiffTimeToSeconds $ diffUTCTime doneTime startTime
      liftIO $ putStrLn ("total time to render: " ++ show deltaTime)
      pure (Break, ())
    RequestInfo -> do
      broadcast (InfoExamples 1)
      pure (Break, ())
    _ -> error "Received a message which I don't know how to handle!"

    where
      eval :: SourceFile -> StylishText
      eval e = applyStyles e stylesheet $ showStylish [] e

stylesheetMain :: Stylesheet -> IO ()
stylesheetMain stylesheet = do
  programName <- head <$> getArgs
  program <- Text.readFile programName
  putStrLn "listening on port 1234!"
  serverMain () (talk programName program stylesheet)
