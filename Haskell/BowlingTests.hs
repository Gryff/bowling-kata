module BowlingTests where 

import Bowling 

import Test.Hspec
import Test.QuickCheck
import Text.Printf (printf)

testScoreGame :: String -> Int -> Spec
testScoreGame game score =
    it (printf "should return the score for game : %s --> %d \n" game score) $
        scoreGame game `shouldBe` score

main = hspec $ do 
    describe "scoreGame" $ do 
        testScoreGame "9-9-9-9-9-9-9-9-9-9-" 90