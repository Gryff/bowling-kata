module Bowling where 

import Data.Char

data Frame = Strike | Spare Char | Simple Char Char
    deriving (Eq, Show)

type FrameNumber = Int


scoreGame :: String -> Int 
scoreGame game = addFrames $ zip [1..] $ parse game

parse :: String -> [Frame]
parse [] = []
parse (x:[])
    | x == 'X' = [Strike]
    | otherwise = [Simple x '-']
parse (x:y:z:[])
    | x == 'X' = [Strike] ++ parse [y] ++ parse [z]
    | otherwise = parse [x,y] ++ parse [z]
parse (x:y:frames)
    | x == 'X' = Strike : parse(y:frames)
    | y == '/' = Spare x : parse frames
    | otherwise = Simple x y : parse frames

addFrames :: [(FrameNumber, Frame)] -> Int
addFrames [] = 0
addFrames (frame:[]) = frameValue frame       
addFrames (frame1:frame2:[]) = 
    case frame1 of 
        (10,Spare x) -> 10 + bonusPoints "Spare" [frame2]
        (_,Spare x) -> 10 + (bonusPoints "Spare" [frame2]) + addFrames [frame2]
        (_,Simple x y) -> frameValue frame1 + addFrames [frame2]
addFrames (frame1:frame2:frame3:[]) = 
    case frame1 of 
        (10, Strike) -> 10 + bonusPoints "Strike" [frame2,frame3]
        (_, Strike) -> 10 + (bonusPoints "Strike" [frame2,frame3]) + addFrames [frame2,frame3]
        (_,Spare _) -> 10 + (bonusPoints "Spare" [frame2]) + addFrames [frame2,frame3]
        (_,Simple _ _) -> frameValue frame1 + addFrames [frame2,frame3]
addFrames (frame1:frame2:frame3:frames) = 
    case frame1 of
        (_,Strike) -> 10 + (bonusPoints "Strike" [frame2,frame3]) + addFrames(frame2:frame3:frames)
        (_,Spare _) -> 10 + (bonusPoints "Spare" [frame2]) + addFrames(frame2:frame3:frames)
        (_,Simple _ _) -> frameValue frame1 + addFrames(frame2:frame3:frames)

frameValue :: (FrameNumber,Frame) -> Int
frameValue (_, Strike) = 10
frameValue (_, Spare x) = rollValue x
frameValue (_, Simple x y) = rollValue x + rollValue y

rollValue :: Char -> Int 
rollValue x = if x == '-' then 0 else digitToInt x

bonusPoints :: String -> [(FrameNumber,Frame)] -> Int
bonusPoints bowlType frames =
    case bowlType of 
        "Strike" -> getTwoRolls frames 
        "Spare" -> getOneRoll $ frames !! 0

getTwoRolls :: [(FrameNumber,Frame)] -> Int 
getTwoRolls (frame1:frame2:[]) = 
    case frame1 of 
        (_,Strike) -> 10 + getOneRoll frame2
        (_,Spare _) -> 10
        (11,Simple _ _) -> getOneRoll frame1 + getOneRoll frame2
        (_,Simple _ _) -> frameValue frame1

getOneRoll :: (FrameNumber,Frame) -> Int 
getOneRoll (_,Strike) = 10
getOneRoll (n,Spare x) = frameValue (n,Spare x)
getOneRoll (_,Simple x _) = rollValue x