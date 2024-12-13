module Main where

import TinyCommon

css :: String
css =
  ".outer-box {\
  \  position: relative;\
  \  display: flex;\
  \  color: black;\
  \  flex-direction: row;\
  \}\
  \.inner-box {\
  \  max-height: 4em;\
  \  overflow: auto;\
  \  border: 2px solid gray;\
  \  border-radius: 3px;\
  \}\
  \.tooltip {\
  \  position: absolute;\
  \  border-style: solid;\
  \  border-color: transparent gray transparent transparent;\
  \  left: -8px;\
  \  top: calc(0.5em - 4px);\
  \  border-width: 8px;\
  \}\
  \.tooltip-spacer {\
  \  width: 5px;\
  \}\
  \.table {\
  \  border-collapse: collapse;\
  \  border-spacing: 0;\
  \}\
  \.even-col {\
  \  background-color: #D7E3F4;\
  \}\
  \.even-col-header {\
  \  position: sticky;\
  \  top: 0;\
  \  background-color: #D7E3F4;\
  \  box-shadow: 0 2px 0 gray;\
  \}\
  \.odd-col-header {\
  \  position: sticky;\
  \  top: 0;\
  \  background-color: white;\
  \  box-shadow: 0 2px 0 gray;\
  \}"

main :: IO ()
main = stylesheetMain
  $ (projectionBoxesStylish @EvalPhase [])
  { stylishTransform = addCSS css }
