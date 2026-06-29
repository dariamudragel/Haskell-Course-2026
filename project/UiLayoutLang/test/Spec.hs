module Main where

import Test.Hspec
import Test.QuickCheck
import UiLayoutLang.Types
import UiLayoutLang.Parser
import UiLayoutLang.Engine

main::IO()
main=hspec$do
 describe "parser"$do
  it "parses example window"$do
   parseWindow "window \"Main\" 800 x 600 { row { box { width:20%; height:100%; color:red; } box { width:80%; height:100%; color:blue; } } }" `shouldSatisfy` isRight

 describe "layout"$do
  it "splits row 20/80"$do
   let Right w=parseWindow "window \"Main\" 800 x 600 { row { box { width:20%; height:100%; } box { width:80%; height:100%; } } }"
   let Resolved _ _ _ _ _ [_,b]=resolveWindow w
   rx b `shouldBe` 160
   rw b `shouldBe` 640

  it "clips overflow"$do
   let lay=Box (defaultProps{pDir=Row}) [Box (defaultProps{pWidth=Px 900}) [],Box (defaultProps{pWidth=Px 900}) []]
   inside (resolveLayout 0 0 800 600 lay) `shouldBe` True

 describe "properties"$do
  it "manual generated row children stay inside"$property$
   \(Positive a) (Positive b)->
    let x=a `mod` 900
        y=b `mod` 900
        lay=Box (defaultProps{pDir=Row}) [Box (defaultProps{pWidth=Px x}) [],Box (defaultProps{pWidth=Px y}) []]
    in inside(resolveLayout 0 0 800 600 lay)

isRight::Either a b->Bool
isRight(Right _)=True
isRight _=False