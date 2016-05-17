module BowlingTests where 

import Bowling 

import Test.Hspec
import Test.QuickCheck
import Text.Printf (printf)

testScoreGame :: String -> Int -> Spec
testScoreGame game score =
    it (printf "should return the score for game : %s --> %d \n" game score) $
        scoreGame game `shouldBe` score
        
testParse :: String -> [Frame] -> Spec
testParse game frames =
    it (printf "should return frames : %s \n" game) $
        parse game `shouldBe` frames

main = hspec $ do 
    describe "scoreGame" $ do 
        testScoreGame"--------------------" 0
        testScoreGame "1-1----------------1" 3
        testScoreGame "9-9-9-9-9-9-9-9-9-9-" 90
        testScoreGame "1-1-1-1-1-1-1-1-1-1-" 10
        testScoreGame "12131415161718171611" 59
        testScoreGame "5/5/5/5/5/5/5/5/5/5/5" 150
        testScoreGame "XXXXXXXXXXXX" 300
        testScoreGame "XXXXXXXXXX12" 274
        testScoreGame "1/35XXX458/X3/23" 160
        testScoreGame "1/35XXX458/X3/XX6" 189
        testScoreGame "5/11------------3/11" 26
    describe "parse" $ do 
        testParse "9-9-9-9-9-9-9-9-9-9-" (replicate 10 (Simple '9' '-'))
        testParse "5/5/5/5/5/5/5/5/5/5/5" (replicate 10 (Spare '5') ++ [Simple '5' '-'])
        testParse "1/35XXX458/X3/23" [Spare '1', Simple '3' '5', Strike, Strike, Strike, Simple '4' '5', Spare '8', Strike, Spare '3', Simple '2' '3']