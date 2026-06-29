module Main where

import System.Environment(getArgs)
import UiLayoutLang.Parser
import UiLayoutLang.Engine
import UiLayoutLang.Render

main::IO()
main=do
 args<-getArgs
 let file=case args of
          []->"examples/main.ui"
          (x:_)->x
 txt<-readFile file
 case parseWindow txt of
  Left e->putStrLn e
  Right w->do
   let r=resolveWindow w
   print r
   writeFile "output.svg"(renderSvg r)
   putStrLn "created output.svg"
