-- module
module CrazyhouseBot
    ( getMove
    , listMoves
    ) 
    where

import Data.Char

import Util

--- external signatures
getMove :: String -> String
getMove string = string



-- Example game situation:
-- "rnbQ2Q1/pppp3p/6k1/8/1P6/8/Pn1pPKPP/RNB2BNR/BPQRppq w"
-- Empty playboard:
-- "00000000/00000000/00000000/00000000/00000000/00000000/00000000/00000000/w"

listMoves :: String -> String
listMoves string = "[" ++ 
                   (prepareListMoves (colorDecider 0 (createBoardWithZeros string) (createBoardWithZeros string))) ++ 
                   "]"

-- ================================================================================
-- UNWRAPPING BOARD (everything's fine until now)
-- ================================================================================
-- Check, if given character CAN BE CONVERTED to Int
isValidInt :: String -> Bool
isValidInt str = str `elem` ["1","2","3","4","5","6","7","8"]

-- Replace number with 0's
repeatZeros :: Int -> String -> String
repeatZeros 0 _ = ""
repeatZeros n str = "0" ++ repeatZeros (n - 1) str 

-- Replace the Char-representation of a number with 0's
extractInt :: String -> String
extractInt str
         | isValidInt str = repeatZeros (read str) ""
         | otherwise = str

-- Call extractInt for each field
createRow :: String -> String
createRow [] = []
createRow (x:xs) = extractInt [x] ++ createRow xs

concatBoard :: [String] -> String
concatBoard [] = []
concatBoard (x:xs) = x ++ "/" ++ concatBoard xs

-- Create the board
createBoardWithZeros :: String -> String 
createBoardWithZeros str = concatBoard (map createRow (init (splitOn '/' str))) ++ last (splitOn '/' str)

-- ================================================================================
-- "GLOBAL" FUNCTIONS (everything's fine until now)
-- ================================================================================

--    A  B  C  D  E  F  G  H
-- 8 00 01 02 03 04 05 06 07 /
-- 7 09 10 11 12 13 14 15 16 /
-- 6 18 19 20 21 22 23 24 25 /
-- 5 27 28 29 30 31 32 33 34 /
-- 4 36 37 38 39 40 41 42 43 /
-- 3 45 46 47 48 49 50 51 52 /
-- 2 54 55 56 57 58 59 60 61 /
-- 1 63 64 65 66 67 68 69 70 /
--    A  B  C  D  E  F  G  H


convertIntToField :: Int -> String
convertIntToField position = head (drop position
                             ["a8","b8","c8","d8","e8","f8","g8","h8","leer",
                              "a7","b7","c7","d7","e7","f7","g7","h7","leer",
                              "a6","b6","c6","d6","e6","f6","g6","h6","leer",
                              "a5","b5","c5","d5","e5","f5","g5","h5","leer",
                              "a4","b4","c4","d4","e4","f4","g4","h4","leer",
                              "a3","b3","c3","d3","e3","f3","g3","h3","leer",
                              "a2","b2","c2","d2","e2","f2","g2","h2","leer",
                              "a1","b1","c1","d1","e1","f1","g1","h1","leer"])


isEmptyField :: Int -> String -> Bool
isEmptyField position board = (head (drop position board)) == '0'

isWhiteEnemy :: Int -> String -> Bool
isWhiteEnemy position board = (head (drop position board)) `elem` "PRNBQK"

isBlackEnemy :: Int -> String -> Bool
isBlackEnemy position board = (head (drop position board)) `elem` "prnbqk"

isLeftEdge :: Int -> Bool
isLeftEdge position = position `elem` [0,9..63]

isUpperEdge :: Int -> Bool
isUpperEdge position = position `elem` [0..7]

isRightEdge :: Int -> Bool
isRightEdge position = position `elem` [7,16..70]

isLowerEdge :: Int -> Bool
isLowerEdge position = position `elem` [63..70]

isWPawn :: Int -> String -> Bool
isWPawn position board = (head (drop position board)) == 'P'

isBPawn :: Int -> String -> Bool
isBPawn position board = (head (drop position board)) == 'p'

isWRook :: Int -> String -> Bool
isWRook position board = (head (drop position board)) `elem` "RQ"

isBRook :: Int -> String -> Bool
isBRook position board = (head (drop position board)) `elem` "rq"

isWKnight :: Int -> String -> Bool
isWKnight position board = (head (drop position board)) == 'N'

isBKnight :: Int -> String -> Bool
isBKnight position board = (head (drop position board)) == 'n'

isWBishop :: Int -> String -> Bool
isWBishop position board = (head (drop position board)) `elem` "BQ"

isBBishop :: Int -> String -> Bool
isBBishop position board = (head (drop position board)) `elem` "bq"

{-
isWQueen :: Int -> String -> Bool
isWQueen position board = (head (drop position board)) == 'Q'

isBQueen :: Int -> String -> Bool
isBQueen position board = (head (drop position board)) == 'q'
-}

isWKing ::Int -> String -> Bool
isWKing position board = (head (drop position board)) == 'K'

isBKing ::Int -> String -> Bool
isBKing position board = (head (drop position board)) == 'k'


findOutKingWPosition :: Int -> String -> String
findOutKingWPosition 71 _ = []
findOutKingWPosition position (x:xs)
                   | x == 'K' = show position
                   | otherwise = findOutKingWPosition (position + 1) xs

findOutKingBPosition :: Int -> String -> String
findOutKingBPosition 71 _ = []
findOutKingBPosition position (x:xs)
                   | x == 'k' = show position
                   | otherwise = findOutKingBPosition (position + 1) xs

overrideOldBoard :: Int -> Int -> String -> String -> String
overrideOldBoard origin moveTo figure board
                        | moveTo == origin = take origin board ++
                                             figure ++
                                             drop (origin + 1) board
                        | moveTo < origin = take moveTo board ++
                                            figure ++
                                            take (origin - moveTo - 1) (drop (moveTo + 1) board) ++
                                            "0" ++
                                            drop (origin + 1) board
                        | otherwise = take origin board ++
                                      "0" ++
                                      take (moveTo - origin - 1) (drop (origin + 1) board) ++
                                      figure ++
                                      drop (moveTo + 1) board

-- ================================================================================
-- COMPUTE ALL CHECK SITUATIONS (everything's fine until now)
-- ================================================================================

checkedByBPawn :: Int -> String -> Bool
checkedByBPawn position board
             | position `elem` [18..70] &&
               not (position `elem` [18,27..63]) &&
               not (position `elem` [25,34..70]) = isBPawn (position - 10) board|| 
                                                   isBPawn (position - 8) board
             | position `elem` [18,27..63] = isBPawn (position - 8) board
             | position `elem` [25,34..70] = isBPawn (position - 10) board
             | otherwise = False

checkedByWPawn :: Int -> String -> Bool
checkedByWPawn position board
             | position `elem` [0..52] &&
               not (position `elem` [0,9..45]) &&
               not (position `elem` [7,16..52]) = isWPawn (position + 10) board|| 
                                                   isWPawn (position + 8) board
             | position `elem` [0,9..45] = isWPawn (position + 10) board
             | position `elem` [7,16..52] = isWPawn (position + 8) board
             | otherwise = False

checkedByBRookLeft :: Int -> String -> Bool
checkedByBRookLeft position board
                 | isBRook position board = True
                 | not (isLeftEdge position) && 
                   isEmptyField position board = checkedByBRookLeft (position - 1) board
                 | otherwise = False

checkedByWRookLeft :: Int -> String -> Bool
checkedByWRookLeft position board
                 | isWRook position board = True
                 | not (isLeftEdge position) && 
                   isEmptyField position board = checkedByWRookLeft (position - 1) board
                 | otherwise = False

checkedByBRookUp :: Int -> String -> Bool
checkedByBRookUp position board
               | isBRook position board = True
               | not (isUpperEdge position) && 
                 isEmptyField position board = checkedByBRookUp (position - 9) board
               | otherwise = False

checkedByWRookUp :: Int -> String -> Bool
checkedByWRookUp position board
               | isWRook position board = True
               | not (isUpperEdge position) && 
                 isEmptyField position board = checkedByWRookUp (position - 9) board
               | otherwise = False

checkedByBRookRight :: Int -> String -> Bool
checkedByBRookRight position board
               | isBRook position board = True
               | not (isRightEdge position) && 
                 isEmptyField position board = checkedByBRookRight (position + 1) board
               | otherwise = False

checkedByWRookRight :: Int -> String -> Bool
checkedByWRookRight position board
               | isWRook position board = True
               | not (isRightEdge position) && 
                 isEmptyField position board = checkedByWRookRight (position + 1) board
               | otherwise = False

checkedByBRookDown :: Int -> String -> Bool
checkedByBRookDown position board
               | isBRook position board = True
               | not (isLowerEdge position) && 
                 isEmptyField position board = checkedByBRookDown (position + 9) board
               | otherwise = False

checkedByWRookDown :: Int -> String -> Bool
checkedByWRookDown position board
               | isWRook position board = True
               | not (isLowerEdge position) && 
                 isEmptyField position board = checkedByWRookDown (position + 9) board
               | otherwise = False

checkedByBRook :: Int -> String -> Bool
checkedByBRook position board
             | not (elem position [0..7]) &&
               not (elem position [63.. 70]) &&
               not (elem position [9,18..54]) &&
               not (elem position [16,25..61]) = checkedByBRookLeft (position - 1) board||
                                                 checkedByBRookUp (position - 9) board ||
                                                 checkedByBRookRight (position + 1) board ||
                                                 checkedByBRookDown (position + 9) board
             | elem position [9,18..54] = checkedByBRookUp (position - 9) board ||
                                          checkedByBRookRight (position + 1) board ||
                                          checkedByBRookDown (position + 9) board
             | elem position [1..6] = checkedByBRookLeft (position - 1) board||
                                      checkedByBRookRight (position + 1) board ||
                                      checkedByBRookDown (position + 9) board
             | elem position [16,25..61] = checkedByBRookLeft (position - 1) board||
                                           checkedByBRookUp (position - 9) board ||
                                           checkedByBRookDown (position + 9) board
             | elem position [64..69] = checkedByBRookLeft (position - 1) board||
                                        checkedByBRookUp (position - 9) board ||
                                        checkedByBRookRight (position + 1) board
             | position == 0 = checkedByBRookRight (position + 1) board ||
                               checkedByBRookDown (position + 9) board
             | position == 7 = checkedByBRookLeft (position - 1) board||
                               checkedByBRookDown (position + 9) board
             | position == 70 = checkedByBRookLeft (position - 1) board||
                                checkedByBRookUp (position - 9) board
             | position == 63 = checkedByBRookUp (position - 9) board ||
                                checkedByBRookRight (position + 1) board
             | otherwise = False

checkedByWRook :: Int -> String -> Bool
checkedByWRook position board
             | not (elem position [0..7]) &&
               not (elem position [63.. 70]) &&
               not (elem position [9,18..54]) &&
               not (elem position [16,25..61]) = checkedByWRookLeft (position - 1) board||
                                                 checkedByWRookUp (position - 9) board ||
                                                 checkedByWRookRight (position + 1) board ||
                                                 checkedByWRookDown (position + 9) board
             | elem position [9,18..54] = checkedByWRookUp (position - 9) board ||
                                          checkedByWRookRight (position + 1) board ||
                                          checkedByWRookDown (position + 9) board
             | elem position [1..6] = checkedByWRookLeft (position - 1) board||
                                      checkedByWRookRight (position + 1) board ||
                                      checkedByWRookDown (position + 9) board
             | elem position [16,25..61] = checkedByWRookLeft (position - 1) board||
                                           checkedByWRookUp (position - 9) board ||
                                           checkedByWRookDown (position + 9) board
             | elem position [64..69] = checkedByWRookLeft (position - 1) board||
                                        checkedByWRookUp (position - 9) board ||
                                        checkedByWRookRight (position + 1) board
             | position == 0 = checkedByWRookRight (position + 1) board ||
                               checkedByWRookDown (position + 9) board
             | position == 7 = checkedByWRookLeft (position - 1) board||
                               checkedByWRookDown (position + 9) board
             | position == 70 = checkedByWRookLeft (position - 1) board||
                                checkedByWRookUp (position - 9) board
             | position == 63 = checkedByWRookUp (position - 9) board ||
                                checkedByWRookRight (position + 1) board
             | otherwise = False

checkedByBKnight :: Int -> String -> Bool
checkedByBKnight position board
               | not (elem position [0..7]) &&
                 not (elem position [9..16]) &&
                 not (elem position [54..61]) &&
                 not (elem position [63..70]) &&
                 not (elem position [18,27,36,45]) &&
                 not (elem position [19,28,37,46]) &&
                 not (elem position [24,33,42,51]) &&
                 not (elem position [25,34,43,52]) = isBKnight (position - 19) board ||
                                                     isBKnight (position - 17) board ||
                                                     isBKnight (position - 11) board ||
                                                     isBKnight (position - 7) board ||
                                                     isBKnight (position + 7) board ||
                                                     isBKnight (position + 11) board ||
                                                     isBKnight (position + 17) board ||
                                                     isBKnight (position + 19) board
               | position `elem` [18,27,36,45] = isBKnight (position - 17) board ||
                                                 isBKnight (position - 7) board ||
                                                 isBKnight (position + 11) board ||
                                                 isBKnight (position + 19) board
               | position `elem` [19,28,37,46] = isBKnight (position - 19) board ||
                                                 isBKnight (position - 17) board ||
                                                 isBKnight (position - 7) board ||
                                                 isBKnight (position + 11) board ||
                                                 isBKnight (position + 17) board ||
                                                 isBKnight (position + 19) board
               | position `elem` [2..5] = isBKnight (position + 7) board ||
                                          isBKnight (position + 11) board ||
                                          isBKnight (position + 17) board ||
                                          isBKnight (position + 19) board
               | position `elem` [11..14] = isBKnight (position - 11) board ||
                                            isBKnight (position - 7) board ||
                                            isBKnight (position + 7) board ||
                                            isBKnight (position + 11) board ||
                                            isBKnight (position + 17) board ||
                                            isBKnight (position + 19) board
               | position `elem` [24,33,42,51] = isBKnight (position - 19) board ||
                                                 isBKnight (position - 17) board ||
                                                 isBKnight (position - 11) board ||
                                                 isBKnight (position + 7) board ||
                                                 isBKnight (position + 17) board ||
                                                 isBKnight (position + 19) board
               | position `elem` [25,34,43,52] = isBKnight (position - 19) board ||
                                                 isBKnight (position - 11) board ||
                                                 isBKnight (position + 7) board ||
                                                 isBKnight (position + 17) board
               | position `elem` [56..59] = isBKnight (position - 19) board ||
                                            isBKnight (position - 17) board ||
                                            isBKnight (position - 11) board ||
                                            isBKnight (position - 7) board ||
                                            isBKnight (position + 7) board ||
                                            isBKnight (position + 11) board
               | position `elem` [65..68] = isBKnight (position - 19) board ||
                                            isBKnight (position - 17) board ||
                                            isBKnight (position - 11) board ||
                                            isBKnight (position - 7) board
               | position == 0 = isBKnight (position + 11) board ||
                                 isBKnight (position + 19) board
               | position == 1 = isBKnight (position + 11) board ||
                                 isBKnight (position + 17) board ||
                                 isBKnight (position + 19) board
               | position == 9 = isBKnight (position - 7) board ||
                                 isBKnight (position + 11) board ||
                                 isBKnight (position + 19) board
               | position == 10 = isBKnight (position - 7) board ||
                                  isBKnight (position + 11) board ||
                                  isBKnight (position + 17) board ||
                                  isBKnight (position + 19) board
               | position == 6 = isBKnight (position + 7) board ||
                                 isBKnight (position + 17) board ||
                                 isBKnight (position + 19) board
               | position == 7 = isBKnight (position + 7) board ||
                                 isBKnight (position + 17) board
               | position == 15 = isBKnight (position - 11) board ||
                                  isBKnight (position + 7) board ||
                                  isBKnight (position + 17) board ||
                                  isBKnight (position + 19) board
               | position == 16 = isBKnight (position - 11) board ||
                                  isBKnight (position + 7) board ||
                                  isBKnight (position + 17) board
               | position == 54 = isBKnight (position - 17) board ||
                                  isBKnight (position - 7) board ||
                                  isBKnight (position + 11) board
               | position == 55 = isBKnight (position - 19) board ||
                                  isBKnight (position - 17) board ||
                                  isBKnight (position - 7) board ||
                                  isBKnight (position + 11) board
               | position == 63 = isBKnight (position - 17) board ||
                                  isBKnight (position - 7) board
               | position == 64 = isBKnight (position - 19) board ||
                                  isBKnight (position - 17) board ||
                                  isBKnight (position - 7) board
               | position == 60 = isBKnight (position - 19) board ||
                                  isBKnight (position - 17) board ||
                                  isBKnight (position - 11) board ||
                                  isBKnight (position + 7) board
               | position == 61 = isBKnight (position - 19) board ||
                                  isBKnight (position - 11) board ||
                                  isBKnight (position + 7) board
               | position == 69 = isBKnight (position - 19) board ||
                                  isBKnight (position - 17) board ||
                                  isBKnight (position - 11) board
               | position == 70 = isBKnight (position - 19) board ||
                                  isBKnight (position - 11) board
               | otherwise = False

checkedByWKnight :: Int -> String -> Bool
checkedByWKnight position board
               | not (elem position [0..7]) &&
                 not (elem position [9..16]) &&
                 not (elem position [54..61]) &&
                 not (elem position [63..70]) &&
                 not (elem position [18,27,36,45]) &&
                 not (elem position [19,28,37,46]) &&
                 not (elem position [24,33,42,51]) &&
                 not (elem position [25,34,43,52]) = isWKnight (position - 19) board ||
                                                     isWKnight (position - 17) board ||
                                                     isWKnight (position - 11) board ||
                                                     isWKnight (position - 7) board ||
                                                     isWKnight (position + 7) board ||
                                                     isWKnight (position + 11) board ||
                                                     isWKnight (position + 17) board ||
                                                     isWKnight (position + 19) board
               | position `elem` [18,27,36,45] = isWKnight (position - 17) board ||
                                                 isWKnight (position - 7) board ||
                                                 isWKnight (position + 11) board ||
                                                 isWKnight (position + 19) board
               | position `elem` [19,28,37,46] = isWKnight (position - 19) board ||
                                                 isWKnight (position - 17) board ||
                                                 isWKnight (position - 7) board ||
                                                 isWKnight (position + 11) board ||
                                                 isWKnight (position + 17) board ||
                                                 isWKnight (position + 19) board
               | position `elem` [2..5] = isWKnight (position + 7) board ||
                                          isWKnight (position + 11) board ||
                                          isWKnight (position + 17) board ||
                                          isWKnight (position + 19) board
               | position `elem` [11..14] = isWKnight (position - 11) board ||
                                            isWKnight (position - 7) board ||
                                            isWKnight (position + 7) board ||
                                            isWKnight (position + 11) board ||
                                            isWKnight (position + 17) board ||
                                            isWKnight (position + 19) board
               | position `elem` [24,33,42,51] = isWKnight (position - 19) board ||
                                                 isWKnight (position - 17) board ||
                                                 isWKnight (position - 11) board ||
                                                 isWKnight (position + 7) board ||
                                                 isWKnight (position + 17) board ||
                                                 isWKnight (position + 19) board
               | position `elem` [25,34,43,52] = isWKnight (position - 19) board ||
                                                 isWKnight (position - 11) board ||
                                                 isWKnight (position + 7) board ||
                                                 isWKnight (position + 17) board
               | position `elem` [56..59] = isWKnight (position - 19) board ||
                                            isWKnight (position - 17) board ||
                                            isWKnight (position - 11) board ||
                                            isWKnight (position - 7) board ||
                                            isWKnight (position + 7) board ||
                                            isWKnight (position + 11) board
               | position `elem` [65..68] = isWKnight (position - 19) board ||
                                            isWKnight (position - 17) board ||
                                            isWKnight (position - 11) board ||
                                            isWKnight (position - 7) board
               | position == 0 = isWKnight (position + 11) board ||
                                 isWKnight (position + 19) board
               | position == 1 = isWKnight (position + 11) board ||
                                 isWKnight (position + 17) board ||
                                 isWKnight (position + 19) board
               | position == 9 = isWKnight (position - 7) board ||
                                 isWKnight (position + 11) board ||
                                 isWKnight (position + 19) board
               | position == 10 = isWKnight (position - 7) board ||
                                  isWKnight (position + 11) board ||
                                  isWKnight (position + 17) board ||
                                  isWKnight (position + 19) board
               | position == 6 = isWKnight (position + 7) board ||
                                 isWKnight (position + 17) board ||
                                 isWKnight (position + 19) board
               | position == 7 = isWKnight (position + 7) board ||
                                 isWKnight (position + 17) board
               | position == 15 = isWKnight (position - 11) board ||
                                  isWKnight (position + 7) board ||
                                  isWKnight (position + 17) board ||
                                  isWKnight (position + 19) board
               | position == 16 = isWKnight (position - 11) board ||
                                  isWKnight (position + 7) board ||
                                  isWKnight (position + 17) board
               | position == 54 = isWKnight (position - 17) board ||
                                  isWKnight (position - 7) board ||
                                  isWKnight (position + 11) board
               | position == 55 = isWKnight (position - 19) board ||
                                  isWKnight (position - 17) board ||
                                  isWKnight (position - 7) board ||
                                  isWKnight (position + 11) board
               | position == 63 = isWKnight (position - 17) board ||
                                  isWKnight (position - 7) board
               | position == 64 = isWKnight (position - 19) board ||
                                  isWKnight (position - 17) board ||
                                  isWKnight (position - 7) board
               | position == 60 = isWKnight (position - 19) board ||
                                  isWKnight (position - 17) board ||
                                  isWKnight (position - 11) board ||
                                  isWKnight (position + 7) board
               | position == 61 = isWKnight (position - 19) board ||
                                  isWKnight (position - 11) board ||
                                  isWKnight (position + 7) board
               | position == 69 = isWKnight (position - 19) board ||
                                  isWKnight (position - 17) board ||
                                  isWKnight (position - 11) board
               | position == 70 = isWKnight (position - 19) board ||
                                  isWKnight (position - 11) board
               | otherwise = False

checkedByBBishopLeftUp :: Int -> String -> Bool
checkedByBBishopLeftUp position board
                     | isBBishop position board = True
                     | (not (isLeftEdge position) &&
                       not (isUpperEdge position)) &&
                       isEmptyField position board = checkedByBBishopLeftUp (position - 10) board
                     | otherwise = False

checkedByWBishopLeftUp :: Int -> String -> Bool
checkedByWBishopLeftUp position board
                     | isWBishop position board = True
                     | (not (isLeftEdge position) &&
                       not (isUpperEdge position)) &&
                       isEmptyField position board = checkedByWBishopLeftUp (position - 10) board
                     | otherwise = False

checkedByBBishopRightUp :: Int -> String -> Bool
checkedByBBishopRightUp position board
                      | isBBishop position board = True
                      | (not (isUpperEdge position) &&
                        not (isRightEdge position)) &&
                        isEmptyField position board = checkedByBBishopRightUp (position - 8) board
                      | otherwise = False

checkedByWBishopRightUp :: Int -> String -> Bool
checkedByWBishopRightUp position board
                      | isWBishop position board = True
                      | (not (isUpperEdge position) &&
                        not (isRightEdge position)) &&
                        isEmptyField position board = checkedByWBishopRightUp (position - 8) board
                      | otherwise = False

checkedByBBishopRightDown :: Int -> String -> Bool
checkedByBBishopRightDown position board
                        | isBBishop position board = True
                        | (not (isRightEdge position) &&
                          not (isLowerEdge position)) &&
                          isEmptyField position board = checkedByBBishopRightDown (position + 10) board
                        | otherwise = False 

checkedByWBishopRightDown :: Int -> String -> Bool
checkedByWBishopRightDown position board
                        | isWBishop position board = True
                        | (not (isRightEdge position) &&
                          not (isLowerEdge position)) &&
                          isEmptyField position board = checkedByWBishopRightDown (position + 10) board
                        | otherwise = False

checkedByBBishopLeftDown :: Int -> String -> Bool
checkedByBBishopLeftDown position board
                       | isBBishop position board = True
                       | (not (isLowerEdge position) &&
                         not (isLeftEdge position)) &&
                         isEmptyField position board = checkedByBBishopLeftDown (position + 8) board
                       | otherwise = False

checkedByWBishopLeftDown :: Int -> String -> Bool
checkedByWBishopLeftDown position board
                       | isWBishop position board = True
                       | (not (isLowerEdge position) &&
                         not (isLeftEdge position)) &&
                         isEmptyField position board = checkedByWBishopLeftDown (position + 8) board
                       | otherwise = False

checkedByBBishop :: Int -> String -> Bool
checkedByBBishop position board
               | not (elem position [0..7]) &&
                 not (elem position [63.. 70]) &&
                 not (elem position [9,18..54]) &&
                 not (elem position [16,25..61]) = checkedByBBishopLeftUp (position - 10) board ||
                                                   checkedByBBishopRightUp (position - 8) board ||
                                                   checkedByBBishopRightDown (position + 10) board ||
                                                   checkedByBBishopLeftDown (position + 8) board
               | position `elem` [9,18..54] = checkedByBBishopRightUp (position - 8) board ||
                                              checkedByBBishopLeftDown (position + 8) board
               | position `elem` [1..6] = checkedByBBishopRightDown (position + 10) board ||
                                          checkedByBBishopLeftDown (position + 8) board
               | position `elem` [16,25..61] = checkedByBBishopLeftUp (position - 10) board ||
                                               checkedByBBishopLeftDown (position + 8) board
               | position `elem` [64..69] = checkedByBBishopLeftUp (position - 10) board ||
                                            checkedByBBishopRightUp (position - 8) board
               | position == 0 = checkedByBBishopRightDown (position + 10) board
               | position == 7 = checkedByBBishopLeftDown (position + 8) board
               | position == 63 = checkedByBBishopRightUp (position - 8) board
               | position == 70 = checkedByBBishopLeftUp (position - 10) board
               | otherwise = False

checkedByWBishop :: Int -> String -> Bool
checkedByWBishop position board
               | not (elem position [0..7]) &&
                 not (elem position [63.. 70]) &&
                 not (elem position [9,18..54]) &&
                 not (elem position [16,25..61]) = checkedByWBishopLeftUp (position - 10) board ||
                                                   checkedByWBishopRightUp (position - 8) board ||
                                                   checkedByWBishopRightDown (position + 10) board ||
                                                   checkedByWBishopLeftDown (position + 8) board
               | position `elem` [9,18..54] = checkedByWBishopRightUp (position - 8) board ||
                                              checkedByWBishopLeftDown (position + 8) board
               | position `elem` [1..6] = checkedByWBishopRightDown (position + 10) board ||
                                          checkedByWBishopLeftDown (position + 8) board
               | position `elem` [16,25..61] = checkedByWBishopLeftUp (position - 10) board ||
                                               checkedByWBishopLeftDown (position + 8) board
               | position `elem` [64..69] = checkedByWBishopLeftUp (position - 10) board ||
                                            checkedByWBishopRightUp (position - 8) board
               | position == 0 = checkedByWBishopRightDown (position + 10) board
               | position == 7 = checkedByWBishopLeftDown (position + 8) board
               | position == 63 = checkedByWBishopRightUp (position - 8) board
               | position == 70 = checkedByWBishopLeftUp (position - 10) board
               | otherwise = False

checkedByBQueen :: Int -> String -> Bool
checkedByBQueen position board
              | (checkedByBRook position board) ||
                (checkedByBBishop position board) = True
              | otherwise = False

checkedByWQueen :: Int -> String -> Bool
checkedByWQueen position board
              | (checkedByWRook position board) ||
                (checkedByWBishop position board) = True
              | otherwise = False

checkedByBKing :: Int -> String -> Bool
checkedByBKing position board
             | not (elem position [0..7]) &&
               not (elem position [63.. 70]) &&
               not (elem position [9,18..54]) &&
               not (elem position [16,25..61]) = isBKing (position - 10) board ||
                                                 isBKing (position - 9) board ||
                                                 isBKing (position - 8) board ||
                                                 isBKing (position - 1) board ||
                                                 isBKing (position + 1) board ||
                                                 isBKing (position + 8) board ||
                                                 isBKing (position + 9) board ||
                                                 isBKing (position + 10) board
             | position `elem` [9,18..54] = isBKing (position - 9) board ||
                                            isBKing (position - 8) board ||
                                            isBKing (position + 1) board ||
                                            isBKing (position + 9) board ||
                                            isBKing (position + 10) board
             | position `elem` [1..6] = isBKing (position - 1) board ||
                                        isBKing (position + 1) board ||
                                        isBKing (position + 8) board ||
                                        isBKing (position + 9) board ||
                                        isBKing (position + 10) board
             | position `elem` [16,25..61] = isBKing (position - 10) board ||
                                             isBKing (position - 9) board ||
                                             isBKing (position - 1) board ||
                                             isBKing (position + 8) board ||
                                             isBKing (position + 9) board
             | position `elem` [64..69] = isBKing (position - 10) board ||
                                          isBKing (position - 9) board ||
                                          isBKing (position - 8) board ||
                                          isBKing (position - 1) board ||
                                          isBKing (position + 1) board
             | position == 0 = isBKing (position + 1) board ||
                               isBKing (position + 9) board ||
                               isBKing (position + 10) board
             | position == 7 = isBKing (position - 1) board ||
                               isBKing (position + 8) board ||
                               isBKing (position + 9) board
             | position == 70 = isBKing (position - 10) board ||
                                isBKing (position - 9) board ||
                                isBKing (position - 1) board
             | position == 63 = isBKing (position - 9) board ||
                                isBKing (position - 8) board ||
                                isBKing (position + 1) board
             | otherwise = False

checkedByWKing :: Int -> String -> Bool
checkedByWKing position board
             | not (elem position [0..7]) &&
               not (elem position [63.. 70]) &&
               not (elem position [9,18..54]) &&
               not (elem position [16,25..61]) = isWKing (position - 10) board ||
                                                 isWKing (position - 9) board ||
                                                 isWKing (position - 8) board ||
                                                 isWKing (position - 1) board ||
                                                 isWKing (position + 1) board ||
                                                 isWKing (position + 8) board ||
                                                 isWKing (position + 9) board ||
                                                 isWKing (position + 10) board
             | position `elem` [9,18..54] = isWKing (position - 9) board ||
                                            isWKing (position - 8) board ||
                                            isWKing (position + 1) board ||
                                            isWKing (position + 9) board ||
                                            isWKing (position + 10) board
             | position `elem` [1..6] = isWKing (position - 1) board ||
                                        isWKing (position + 1) board ||
                                        isWKing (position + 8) board ||
                                        isWKing (position + 9) board ||
                                        isWKing (position + 10) board
             | position `elem` [16,25..61] = isWKing (position - 10) board ||
                                             isWKing (position - 9) board ||
                                            isWKing (position - 1) board ||
                                             isWKing (position + 8) board ||
                                             isWKing (position + 9) board
             | position `elem` [64..69] = isWKing (position - 10) board ||
                                          isWKing (position - 9) board ||
                                          isWKing (position - 8) board ||
                                          isWKing (position - 1) board ||
                                          isWKing (position + 1) board
             | position == 0 = isWKing (position + 1) board ||
                               isWKing (position + 9) board ||
                               isWKing (position + 10) board
             | position == 7 = isWKing (position - 1) board ||
                               isWKing (position + 8) board ||
                               isWKing (position + 9) board
             | position == 70 = isWKing (position - 10) board ||
                                isWKing (position - 9) board ||
                                isWKing (position - 1) board
             | position == 63 = isWKing (position - 9) board ||
                                isWKing (position - 8) board ||
                                isWKing (position + 1) board
             | otherwise = False

checkedByBFigure :: Int -> String -> Bool
checkedByBFigure position board
               | checkedByBPawn position board ||
                 checkedByBRook position board ||
                 checkedByBKnight position board ||
                 checkedByBBishop position board ||
                 checkedByBQueen position board ||
                 checkedByBKing position board = True
               | otherwise = False

checkedByWFigure :: Int -> String -> Bool
checkedByWFigure position board
               | checkedByWPawn position board ||
                 checkedByWRook position board ||
                 checkedByWKnight position board ||
                 checkedByWBishop position board ||
                 checkedByWQueen position board ||
                 checkedByWKing position board = True
               | otherwise = False

-- ================================================================================
-- COMPUTE ALL PAWN MOVES WITH CHECK DETECTION (everything's fine until now)
-- ================================================================================

movePawnWLeft :: Int -> String -> String
movePawnWLeft position board 
            | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard position (position - 10) "P" board)) &&
              isBlackEnemy (position - 10) board = "," ++
                                                   convertIntToField position ++
                                                   "-" ++
                                                   convertIntToField (position - 10)
            | otherwise = []

movePawnBLeft :: Int -> String -> String
movePawnBLeft position board 
            | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard position (position + 8) "p" board)) &&
              isWhiteEnemy (position + 8) board = "," ++
                                                  convertIntToField position ++
                                                  "-" ++
                                                  convertIntToField (position + 8)
            | otherwise = []

movePawnWMiddle :: Int -> String -> String
movePawnWMiddle position board
              | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard position (position - 9) "P" board)) &&
                isEmptyField (position - 9) board = "," ++
                                                    convertIntToField position ++
                                                    "-" ++
                                                    convertIntToField (position - 9)
              | otherwise = []

movePawnBMiddle :: Int -> String -> String
movePawnBMiddle position board
              | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard position (position + 9) "p" board)) &&
                isEmptyField (position + 9) board = "," ++
                                                    convertIntToField position ++
                                                    "-" ++
                                                    convertIntToField (position + 9)
              | otherwise = []

movePawnWRight :: Int -> String -> String
movePawnWRight position board
            | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard position (position - 8) "P" board)) &&
              isBlackEnemy (position - 8) board = "," ++
                                                  convertIntToField position ++
                                                  "-" ++
                                                  convertIntToField (position - 8)
            | otherwise = []

movePawnBRight :: Int -> String -> String
movePawnBRight position board
            | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard position (position + 10) "p" board)) &&
              isWhiteEnemy (position + 10) board = "," ++
                                                   convertIntToField position ++
                                                   "-" ++
                                                   convertIntToField (position + 10)
            | otherwise = []

movePawnWDouble :: Int -> String -> String
movePawnWDouble position board
              | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard position (position - 18) "P" board)) &&
                position `elem` [54..61] &&
                isEmptyField (position - 9) board &&
                isEmptyField (position - 18) board = "," ++
                                                     convertIntToField position ++
                                                     "-" ++
                                                     convertIntToField (position - 18)
              | otherwise = []

movePawnBDouble :: Int -> String -> String
movePawnBDouble position board
              | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard position (position + 18) "p" board)) &&
                position `elem` [9..16] &&
                isEmptyField (position + 9) board &&
                isEmptyField (position + 18) board = "," ++
                                                     convertIntToField position ++
                                                     "-" ++
                                                     convertIntToField (position + 18)
              | otherwise = []

doMovePawnW :: Int -> String -> String
doMovePawnW position board
          | position `elem` [0..7] = []
          | position `elem` [9..61] && 
            not (elem position [16,25..61]) &&
            not (elem position [9,18..54]) = (movePawnWLeft position board) ++
                                             (movePawnWMiddle position board) ++
                                             (movePawnWRight position board) ++
                                             (movePawnWDouble position board)
          | position `elem` [9..61] && 
            elem position [16,25..61] = (movePawnWLeft position board) ++
                                        (movePawnWMiddle position board) ++
                                        (movePawnWDouble position board)
          | position `elem` [9..61] && 
            elem position [9,18..54] = (movePawnWMiddle position board) ++
                                       (movePawnWRight position board) ++
                                       (movePawnWDouble position board)
          | otherwise = []


doMovePawnB :: Int -> String -> String
doMovePawnB position board
          | position `elem` [63..70] = []
          | position `elem` [9..61] && 
            not (elem position [16,25..61]) &&
            not (elem position [9,18..54]) = (movePawnBLeft position board) ++
                                             (movePawnBMiddle position board) ++
                                             (movePawnBRight position board) ++
                                             (movePawnBDouble position board)
          | position `elem` [9..61] && 
            elem position [16,25..61] = (movePawnBLeft position board) ++
                                        (movePawnBMiddle position board) ++
                                        (movePawnBDouble position board)
          | position `elem` [9..61] && 
            elem position [9,18..54] = (movePawnBMiddle position board) ++
                                       (movePawnBRight position board) ++
                                       (movePawnBDouble position board)
          | otherwise = []

-- ================================================================================
-- COMPUTE ALL ROOK MOVES WITH CHECK DETECTION (everything's fine until now)
-- ================================================================================

moveRookWLeft :: Int -> Int -> String -> String
moveRookWLeft origin moveTo board 
            | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "R" board)) &&
              isBlackEnemy moveTo board = "," ++ 
                                          convertIntToField origin ++ 
                                          "-" ++ 
                                          convertIntToField moveTo
            | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "R" board)) &&
              isLeftEdge moveTo &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo
            | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "R" board)) &&
              not (isLeftEdge moveTo) &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo ++
                                          moveRookWLeft origin (moveTo - 1) board
            | not (isLeftEdge moveTo) &&
              isEmptyField moveTo board = moveRookWLeft origin (moveTo - 1) board
            | otherwise = []

moveRookBLeft :: Int -> Int -> String -> String
moveRookBLeft origin moveTo board 
            | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "r" board)) &&
              isWhiteEnemy moveTo board = "," ++ 
                                          convertIntToField origin ++ 
                                          "-" ++ 
                                          convertIntToField moveTo
            | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "r" board)) &&
              isLeftEdge moveTo &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo
            | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "r" board)) &&
              not (isLeftEdge moveTo) &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo ++
                                          moveRookBLeft origin (moveTo - 1) board
            | not (isLeftEdge moveTo) &&
              isEmptyField moveTo board = moveRookBLeft origin (moveTo - 1) board
            | otherwise = []

moveRookWUp :: Int -> Int -> String -> String
moveRookWUp origin moveTo board 
            | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "R" board)) &&
              isBlackEnemy moveTo board = "," ++ 
                                          convertIntToField origin ++ 
                                          "-" ++ 
                                          convertIntToField moveTo
            | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "R" board)) &&
              isUpperEdge moveTo &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo
            | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "R" board)) &&
              not (isUpperEdge moveTo) &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo ++
                                          moveRookWUp origin (moveTo - 9) board
            | not(isUpperEdge moveTo) &&
              isEmptyField moveTo board = moveRookWUp origin (moveTo - 9) board
            | otherwise = []

moveRookBUp :: Int -> Int -> String -> String
moveRookBUp origin moveTo board 
            | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "r" board)) &&
              isWhiteEnemy moveTo board = "," ++ 
                                          convertIntToField origin ++ 
                                          "-" ++ 
                                          convertIntToField moveTo
            | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "r" board)) &&
              isUpperEdge moveTo &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo
            | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "r" board)) &&
              not (isUpperEdge moveTo) &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo ++
                                          moveRookBUp origin (moveTo - 9) board
            | not (isUpperEdge moveTo) &&
              isEmptyField moveTo board = moveRookBUp origin (moveTo - 9) board
            | otherwise = []

moveRookWRight :: Int -> Int -> String -> String
moveRookWRight origin moveTo board 
            | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "R" board)) &&
              isBlackEnemy moveTo board = "," ++ 
                                          convertIntToField origin ++ 
                                          "-" ++ 
                                          convertIntToField moveTo
            | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "R" board)) &&
              isRightEdge moveTo &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo
            | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "R" board)) &&
              not (isRightEdge moveTo) &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo ++
                                          moveRookWRight origin (moveTo + 1) board
            | not (isRightEdge moveTo) &&
              isEmptyField moveTo board = moveRookWRight origin (moveTo + 1) board
            | otherwise = []

moveRookBRight :: Int -> Int -> String -> String
moveRookBRight origin moveTo board 
            | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "r" board)) &&
              isWhiteEnemy moveTo board = "," ++ 
                                          convertIntToField origin ++ 
                                          "-" ++ 
                                          convertIntToField moveTo
            | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "r" board)) &&
              isRightEdge moveTo &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo
            | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "r" board)) &&
              not (isRightEdge moveTo) &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo ++
                                          moveRookBRight origin (moveTo + 1) board
            | not (isRightEdge moveTo) &&
              isEmptyField moveTo board = moveRookBRight origin (moveTo + 1) board
            | otherwise = []

moveRookWDown :: Int -> Int -> String -> String
moveRookWDown origin moveTo board 
            | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "R" board)) &&
              isBlackEnemy moveTo board = "," ++ 
                                          convertIntToField origin ++ 
                                          "-" ++ 
                                          convertIntToField moveTo
            | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "R" board)) &&
              isLowerEdge moveTo &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo
            | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "R" board)) &&
              not (isLowerEdge moveTo) &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo ++
                                          moveRookWDown origin (moveTo + 9) board
            | not (isLowerEdge moveTo) &&
              isEmptyField moveTo board = moveRookWDown origin (moveTo + 9) board
            | otherwise = []

moveRookBDown :: Int -> Int -> String -> String
moveRookBDown origin moveTo board 
            | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "r" board)) &&
              isWhiteEnemy moveTo board = "," ++ 
                                          convertIntToField origin ++ 
                                          "-" ++ 
                                          convertIntToField moveTo
            | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "r" board)) &&
              isLowerEdge moveTo &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo
            | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "r" board)) &&
              not (isLowerEdge moveTo) &&
              isEmptyField moveTo board = "," ++
                                          convertIntToField origin ++
                                          "-" ++
                                          convertIntToField moveTo ++
                                          moveRookBDown origin (moveTo + 9) board
            | not (isLowerEdge moveTo) &&
              isEmptyField moveTo board = moveRookBDown origin (moveTo + 9) board
            | otherwise = []

doMoveRookW :: Int -> String -> String
doMoveRookW position board
          | not (elem position [0..7]) &&
            not (elem position [63.. 70]) &&
            not (elem position [9,18..54]) &&
            not (elem position [16,25..61]) = (moveRookWLeft position (position - 1) board) ++
                                              (moveRookWUp position (position - 9) board) ++
                                              (moveRookWRight position (position + 1) board) ++
                                              (moveRookWDown position (position + 9) board)
          | position `elem` [16,25..61] = (moveRookWLeft position (position - 1) board) ++
                                          (moveRookWUp position (position - 9) board) ++
                                          (moveRookWDown position (position + 9) board)
          | position `elem` [9,18..54] = (moveRookWUp position (position - 9) board) ++
                                         (moveRookWRight position (position + 1) board) ++
                                         (moveRookWDown position (position + 9) board)
          | position `elem` [64..69] = (moveRookWLeft position (position - 1) board) ++
                                       (moveRookWUp position (position - 9) board) ++
                                       (moveRookWRight position (position + 1) board)
          | position `elem` [1..6] = (moveRookWLeft position (position - 1) board) ++
                                     (moveRookWRight position (position + 1) board) ++
                                     (moveRookWDown position (position + 9) board)
          | position == 0 = (moveRookWRight position (position + 1) board) ++
                            (moveRookWDown position (position + 9) board)
          | position == 7 = (moveRookWLeft position (position - 1) board) ++
                            (moveRookWDown position (position + 9) board)
          | position == 63 = (moveRookWUp position (position - 9) board) ++
                             (moveRookWRight position (position + 1) board)
          | position == 70 = (moveRookWLeft position (position - 1) board) ++
                             (moveRookWUp position (position - 9) board)
          | otherwise = []

doMoveRookB :: Int -> String -> String
doMoveRookB position board
          | not (elem position [0..7]) &&
            not (elem position [63.. 70]) &&
            not (elem position [9,18..54]) &&
            not (elem position [16,25..61]) = (moveRookBLeft position (position - 1) board) ++
                                              (moveRookBUp position (position - 9) board) ++
                                              (moveRookBRight position (position + 1) board) ++
                                              (moveRookBDown position (position + 9) board)
          | position `elem` [16,25..61] = (moveRookBLeft position (position - 1) board) ++
                                          (moveRookBUp position (position - 9) board) ++
                                          (moveRookBDown position (position + 9) board)
          | position `elem` [9,18..54] = (moveRookBUp position (position - 9) board) ++
                                         (moveRookBRight position (position + 1) board) ++
                                         (moveRookBDown position (position + 9) board)
          | position `elem` [64..69] = (moveRookBLeft position (position - 1) board) ++
                                       (moveRookBUp position (position - 9) board) ++
                                       (moveRookBRight position (position + 1) board)
          | position `elem` [1..6] = (moveRookBLeft position (position - 1) board) ++
                                     (moveRookBRight position (position + 1) board) ++
                                     (moveRookBDown position (position + 9) board)
          | position == 0 = (moveRookBRight position (position + 1) board) ++
                            (moveRookBDown position (position + 9) board)
          | position == 7 = (moveRookBLeft position (position - 1) board) ++
                            (moveRookBDown position (position + 9) board)
          | position == 63 = (moveRookBUp position (position - 9) board) ++
                             (moveRookBRight position (position + 1) board)
          | position == 70 = (moveRookBLeft position (position - 1) board) ++
                             (moveRookBUp position (position - 9) board)
          | otherwise = []

-- ================================================================================
-- COMPUTE ALL KNIGHT MOVES WITH CHECK DETECTION (everything's fine until now)
-- ================================================================================

moveKnightW :: Int -> Int -> String -> String
moveKnightW origin moveTo board
          | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "N" board)) &&
            isBlackEnemy moveTo board = "," ++ 
                                        convertIntToField origin ++ 
                                        "-" ++ 
                                        convertIntToField moveTo
          | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "N" board)) &&
            isEmptyField moveTo board = "," ++ 
                                        convertIntToField origin ++ 
                                        "-" ++ 
                                        convertIntToField moveTo
          | otherwise = []

moveKnightB :: Int -> Int -> String -> String
moveKnightB origin moveTo board
          | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "n" board)) &&
            isWhiteEnemy moveTo board = "," ++ 
                                        convertIntToField origin ++ 
                                        "-" ++ 
                                        convertIntToField moveTo
          | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "n" board)) &&
            isEmptyField moveTo board = "," ++ 
                                        convertIntToField origin ++ 
                                        "-" ++ 
                                        convertIntToField moveTo
          | otherwise = []

doMoveKnightW :: Int -> String -> String
doMoveKnightW position board
            | not (elem position [0..7]) &&
              not (elem position [9..16]) &&
              not (elem position [54..61]) &&
              not (elem position [63..70]) &&
              not (elem position [18,27,36,45]) &&
              not (elem position [19,28,37,46]) &&
              not (elem position [24,33,42,51]) &&
              not (elem position [25,34,43,52]) = moveKnightW position (position - 19) board ++
                                                  moveKnightW position (position - 17) board ++
                                                  moveKnightW position (position - 11) board ++
                                                  moveKnightW position (position - 7) board ++
                                                  moveKnightW position (position + 7) board ++
                                                  moveKnightW position (position + 11) board ++
                                                  moveKnightW position (position + 17) board ++
                                                  moveKnightW position (position + 19) board
            | position `elem` [18,27,36,45] = moveKnightW position (position - 17) board ++
                                              moveKnightW position (position - 7) board ++
                                              moveKnightW position (position + 11) board ++
                                              moveKnightW position (position + 19) board
            | position `elem` [19,28,37,46] = moveKnightW position (position - 19) board ++
                                              moveKnightW position (position - 17) board ++
                                              moveKnightW position (position - 7) board ++
                                              moveKnightW position (position + 11) board ++
                                              moveKnightW position (position + 17) board ++
                                              moveKnightW position (position + 19) board
            | position `elem` [2..5] = moveKnightW position (position + 7) board ++
                                       moveKnightW position (position + 11) board ++
                                       moveKnightW position (position + 17) board ++
                                       moveKnightW position (position + 19) board
            | position `elem` [11..14] = moveKnightW position (position - 11) board ++
                                         moveKnightW position (position - 7) board ++
                                         moveKnightW position (position + 7) board ++
                                         moveKnightW position (position + 11) board ++
                                         moveKnightW position (position + 17) board ++
                                         moveKnightW position (position + 19) board
            | position `elem` [24,33,42,51] = moveKnightW position (position - 19) board ++
                                              moveKnightW position (position - 17) board ++
                                              moveKnightW position (position - 11) board ++
                                              moveKnightW position (position + 7) board ++
                                              moveKnightW position (position + 17) board ++
                                              moveKnightW position (position + 19) board
            | position `elem` [25,34,43,52] = moveKnightW position (position - 19) board ++
                                              moveKnightW position (position - 11) board ++
                                              moveKnightW position (position + 7) board ++
                                              moveKnightW position (position + 17) board
            | position `elem` [56..59] = moveKnightW position (position - 19) board ++
                                         moveKnightW position (position - 17) board ++
                                         moveKnightW position (position - 11) board ++
                                         moveKnightW position (position - 7) board ++
                                         moveKnightW position (position + 7) board ++
                                         moveKnightW position (position + 11) board
            | position `elem` [65..68] = moveKnightW position (position - 19) board ++
                                         moveKnightW position (position - 17) board ++
                                         moveKnightW position (position - 11) board ++
                                         moveKnightW position (position - 7) board
            | position == 0 = moveKnightW position (position + 11) board ++
                              moveKnightW position (position + 19) board
            | position == 1 = moveKnightW position (position + 11) board ++
                              moveKnightW position (position + 17) board ++
                              moveKnightW position (position + 19) board
            | position == 9 = moveKnightW position (position - 7) board ++
                              moveKnightW position (position + 11) board ++
                              moveKnightW position (position + 19) board
            | position == 10 = moveKnightW position (position - 7) board ++
                               moveKnightW position (position + 11) board ++
                               moveKnightW position (position + 17) board ++
                               moveKnightW position (position + 19) board
            | position == 6 = moveKnightW position (position + 7) board ++
                              moveKnightW position (position + 17) board ++
                              moveKnightW position (position + 19) board
            | position == 7 = moveKnightW position (position + 7) board ++
                              moveKnightW position (position + 17) board
            | position == 15 = moveKnightW position (position - 11) board ++
                               moveKnightW position (position + 7) board ++
                               moveKnightW position (position + 17) board ++
                               moveKnightW position (position + 19) board
            | position == 16 = moveKnightW position (position - 11) board ++
                               moveKnightW position (position + 7) board ++
                               moveKnightW position (position + 17) board
            | position == 54 = moveKnightW position (position - 17) board ++
                               moveKnightW position (position - 7) board ++
                               moveKnightW position (position + 11) board
            | position == 55 = moveKnightW position (position - 19) board ++
                               moveKnightW position (position - 17) board ++
                               moveKnightW position (position - 7) board ++
                               moveKnightW position (position + 11) board
            | position == 63 = moveKnightW position (position - 17) board ++
                               moveKnightW position (position - 7) board
            | position == 64 = moveKnightW position (position - 19) board ++
                               moveKnightW position (position - 17) board ++
                               moveKnightW position (position - 7) board
            | position == 60 = moveKnightW position (position - 19) board ++
                               moveKnightW position (position - 17) board ++
                               moveKnightW position (position - 11) board ++
                               moveKnightW position (position + 7) board
            | position == 61 = moveKnightW position (position - 19) board ++
                               moveKnightW position (position - 11) board ++
                               moveKnightW position (position + 7) board
            | position == 69 = moveKnightW position (position - 19) board ++
                               moveKnightW position (position - 17) board ++
                               moveKnightW position (position - 11) board
            | position == 70 = moveKnightW position (position - 19) board ++
                               moveKnightW position (position - 11) board
            | otherwise = []

doMoveKnightB :: Int -> String -> String
doMoveKnightB position board
            | not (elem position [0..7]) &&
              not (elem position [9..16]) &&
              not (elem position [54..61]) &&
              not (elem position [63..70]) &&
              not (elem position [18,27,36,45]) &&
              not (elem position [19,28,37,46]) &&
              not (elem position [24,33,42,51]) &&
              not (elem position [25,34,43,52]) = moveKnightB position (position - 19) board ++
                                                  moveKnightB position (position - 17) board ++
                                                  moveKnightB position (position - 11) board ++
                                                  moveKnightB position (position - 7) board ++
                                                  moveKnightB position (position + 7) board ++
                                                  moveKnightB position (position + 11) board ++
                                                  moveKnightB position (position + 17) board ++
                                                  moveKnightB position (position + 19) board
            | position `elem` [18,27,36,45] = moveKnightB position (position - 17) board ++
                                              moveKnightB position (position - 7) board ++
                                              moveKnightB position (position + 11) board ++
                                              moveKnightB position (position + 19) board
            | position `elem` [19,28,37,46] = moveKnightB position (position - 19) board ++
                                              moveKnightB position (position - 17) board ++
                                              moveKnightB position (position - 7) board ++
                                              moveKnightB position (position + 11) board ++
                                              moveKnightB position (position + 17) board ++
                                              moveKnightB position (position + 19) board
            | position `elem` [2..5] = moveKnightB position (position + 7) board ++
                                       moveKnightB position (position + 11) board ++
                                       moveKnightB position (position + 17) board ++
                                       moveKnightB position (position + 19) board
            | position `elem` [11..14] = moveKnightB position (position - 11) board ++
                                         moveKnightB position (position - 7) board ++
                                         moveKnightB position (position + 7) board ++
                                         moveKnightB position (position + 11) board ++
                                         moveKnightB position (position + 17) board ++
                                         moveKnightB position (position + 19) board
            | position `elem` [24,33,42,51] = moveKnightB position (position - 19) board ++
                                              moveKnightB position (position - 17) board ++
                                              moveKnightB position (position - 11) board ++
                                              moveKnightB position (position + 7) board ++
                                              moveKnightB position (position + 17) board ++
                                              moveKnightB position (position + 19) board
            | position `elem` [25,34,43,52] = moveKnightB position (position - 19) board ++
                                              moveKnightB position (position - 11) board ++
                                              moveKnightB position (position + 7) board ++
                                              moveKnightB position (position + 17) board
            | position `elem` [56..59] = moveKnightB position (position - 19) board ++
                                         moveKnightB position (position - 17) board ++
                                         moveKnightB position (position - 11) board ++
                                         moveKnightB position (position - 7) board ++
                                         moveKnightB position (position + 7) board ++
                                         moveKnightB position (position + 11) board
            | position `elem` [65..68] = moveKnightB position (position - 19) board ++
                                         moveKnightB position (position - 17) board ++
                                         moveKnightB position (position - 11) board ++
                                         moveKnightB position (position - 7) board
            | position == 0 = moveKnightB position (position + 11) board ++
                              moveKnightB position (position + 19) board
            | position == 1 = moveKnightB position (position + 11) board ++
                              moveKnightB position (position + 17) board ++
                              moveKnightB position (position + 19) board
            | position == 9 = moveKnightB position (position - 7) board ++
                              moveKnightB position (position + 11) board ++
                              moveKnightB position (position + 19) board
            | position == 10 = moveKnightB position (position - 7) board ++
                               moveKnightB position (position + 11) board ++
                               moveKnightB position (position + 17) board ++
                               moveKnightB position (position + 19) board
            | position == 6 = moveKnightB position (position + 7) board ++
                              moveKnightB position (position + 17) board ++
                              moveKnightB position (position + 19) board
            | position == 7 = moveKnightB position (position + 7) board ++
                              moveKnightB position (position + 17) board
            | position == 15 = moveKnightB position (position - 11) board ++
                               moveKnightB position (position + 7) board ++
                               moveKnightB position (position + 17) board ++
                               moveKnightB position (position + 19) board
            | position == 16 = moveKnightB position (position - 11) board ++
                               moveKnightB position (position + 7) board ++
                               moveKnightB position (position + 17) board
            | position == 54 = moveKnightB position (position - 17) board ++
                               moveKnightB position (position - 7) board ++
                               moveKnightB position (position + 11) board
            | position == 55 = moveKnightB position (position - 19) board ++
                               moveKnightB position (position - 17) board ++
                               moveKnightB position (position - 7) board ++
                               moveKnightB position (position + 11) board
            | position == 63 = moveKnightB position (position - 17) board ++
                               moveKnightB position (position - 7) board
            | position == 64 = moveKnightB position (position - 19) board ++
                               moveKnightB position (position - 17) board ++
                               moveKnightB position (position - 7) board
            | position == 60 = moveKnightB position (position - 19) board ++
                               moveKnightB position (position - 17) board ++
                               moveKnightB position (position - 11) board ++
                               moveKnightB position (position + 7) board
            | position == 61 = moveKnightB position (position - 19) board ++
                               moveKnightB position (position - 11) board ++
                               moveKnightB position (position + 7) board
            | position == 69 = moveKnightB position (position - 19) board ++
                               moveKnightB position (position - 17) board ++
                               moveKnightB position (position - 11) board
            | position == 70 = moveKnightB position (position - 19) board ++
                               moveKnightB position (position - 11) board
            | otherwise = []

-- ================================================================================
-- COMPUTE ALL BISHOP MOVES WITH CHECK DETECTION (everything's fine until now)
-- ================================================================================    

moveBishopWLeftUp :: Int -> Int -> String -> String
moveBishopWLeftUp origin moveTo board
                | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "B" board)) &&
                  isBlackEnemy moveTo board = "," ++ 
                                              convertIntToField origin ++ 
                                              "-" ++ 
                                              convertIntToField moveTo
                | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "B" board)) &&
                  (isLeftEdge moveTo ||
                  isUpperEdge moveTo) &&
                  isEmptyField moveTo board = "," ++
                                              convertIntToField origin ++
                                              "-" ++
                                              convertIntToField moveTo
                | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "B" board)) &&
                  not (isLeftEdge moveTo) &&
                  not (isUpperEdge moveTo) &&
                  isEmptyField moveTo board = "," ++
                                              convertIntToField origin ++
                                              "-" ++
                                              convertIntToField moveTo ++
                                              moveBishopWLeftUp origin (moveTo - 10) board
                | not (isLeftEdge moveTo) &&
                  not (isUpperEdge moveTo) &&
                  isEmptyField moveTo board = moveBishopWLeftUp origin (moveTo - 10) board
                | otherwise = []

moveBishopBLeftUp :: Int -> Int -> String -> String
moveBishopBLeftUp origin moveTo board
                | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "b" board)) &&
                  isWhiteEnemy moveTo board = "," ++ 
                                              convertIntToField origin ++ 
                                              "-" ++ 
                                              convertIntToField moveTo
                | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "b" board)) &&
                  (isLeftEdge moveTo ||
                  isUpperEdge moveTo) &&
                  isEmptyField moveTo board = "," ++
                                              convertIntToField origin ++
                                              "-" ++
                                              convertIntToField moveTo
                | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "b" board)) &&
                  not (isLeftEdge moveTo) &&
                  not (isUpperEdge moveTo) &&
                  isEmptyField moveTo board = "," ++
                                              convertIntToField origin ++
                                              "-" ++
                                              convertIntToField moveTo ++
                                              moveBishopBLeftUp origin (moveTo - 10) board
                | not (isLeftEdge moveTo) &&
                  not (isUpperEdge moveTo) &&
                  isEmptyField moveTo board = moveBishopBLeftUp origin (moveTo - 10) board
                | otherwise = []

moveBishopWRightUp :: Int -> Int -> String -> String
moveBishopWRightUp origin moveTo board
                 | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "B" board)) &&
                   isBlackEnemy moveTo board = "," ++ 
                                               convertIntToField origin ++ 
                                               "-" ++ 
                                               convertIntToField moveTo
                 | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "B" board)) &&
                   (isUpperEdge moveTo ||
                   isRightEdge moveTo) &&
                   isEmptyField moveTo board = "," ++
                                               convertIntToField origin ++
                                               "-" ++
                                               convertIntToField moveTo
                 | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "B" board)) &&
                   not (isUpperEdge moveTo) &&
                   not (isRightEdge moveTo) &&
                   isEmptyField moveTo board = "," ++
                                               convertIntToField origin ++
                                               "-" ++
                                               convertIntToField moveTo ++
                                               moveBishopWRightUp origin (moveTo - 8) board
                 | not (isUpperEdge moveTo) &&
                   not (isRightEdge moveTo) &&
                   isEmptyField moveTo board = moveBishopWRightUp origin (moveTo - 8) board
                 | otherwise = []

moveBishopBRightUp :: Int -> Int -> String -> String
moveBishopBRightUp origin moveTo board
                 | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "b" board)) &&
                   isWhiteEnemy moveTo board = "," ++ 
                                               convertIntToField origin ++ 
                                               "-" ++ 
                                               convertIntToField moveTo
                 | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "b" board)) &&
                   (isUpperEdge moveTo ||
                   isRightEdge moveTo) &&
                   isEmptyField moveTo board = "," ++
                                               convertIntToField origin ++
                                               "-" ++
                                               convertIntToField moveTo
                 | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "b" board)) &&
                   not (isUpperEdge moveTo) &&
                   not (isRightEdge moveTo) &&
                   isEmptyField moveTo board = "," ++
                                               convertIntToField origin ++
                                               "-" ++
                                               convertIntToField moveTo ++
                                               moveBishopBRightUp origin (moveTo - 8) board
                 | not (isUpperEdge moveTo) &&
                   not (isRightEdge moveTo) &&
                   isEmptyField moveTo board = moveBishopBRightUp origin (moveTo - 8) board
                 | otherwise = []

moveBishopWLeftDown :: Int -> Int -> String -> String
moveBishopWLeftDown origin moveTo board
                  | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "B" board)) &&
                    isBlackEnemy moveTo board = "," ++ 
                                                convertIntToField origin ++ 
                                                "-" ++ 
                                                convertIntToField moveTo
                  | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "B" board)) &&
                    (isLeftEdge moveTo ||
                    isLowerEdge moveTo) &&
                    isEmptyField moveTo board = "," ++
                                                convertIntToField origin ++
                                                "-" ++
                                                convertIntToField moveTo
                  | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "B" board)) &&
                    not (isLeftEdge moveTo) &&
                    not (isLowerEdge moveTo) &&
                    isEmptyField moveTo board = "," ++
                                                convertIntToField origin ++
                                                "-" ++
                                                convertIntToField moveTo ++
                                                moveBishopWLeftDown origin (moveTo + 8) board
                  | not (isLeftEdge moveTo) &&
                    not (isLowerEdge moveTo) &&
                    isEmptyField moveTo board = moveBishopWLeftDown origin (moveTo + 8) board
                  | otherwise = []            

moveBishopBLeftDown :: Int -> Int -> String -> String
moveBishopBLeftDown origin moveTo board
                  | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "b" board)) &&
                    isWhiteEnemy moveTo board = "," ++ 
                                                convertIntToField origin ++ 
                                                "-" ++ 
                                                convertIntToField moveTo
                  | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "b" board)) &&
                    (isLeftEdge moveTo ||
                    isLowerEdge moveTo) &&
                    isEmptyField moveTo board = "," ++
                                                convertIntToField origin ++
                                                "-" ++
                                                convertIntToField moveTo
                  | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "b" board)) &&
                    not (isLeftEdge moveTo) &&
                    not (isLowerEdge moveTo) &&
                    isEmptyField moveTo board = "," ++
                                                convertIntToField origin ++
                                                "-" ++
                                                convertIntToField moveTo ++
                                                moveBishopBLeftDown origin (moveTo + 8) board
                  | not (isLeftEdge moveTo) &&
                    not (isLowerEdge moveTo) &&
                    isEmptyField moveTo board = moveBishopBLeftDown origin (moveTo + 8) board
                  | otherwise = []

moveBishopWRightDown :: Int -> Int -> String -> String
moveBishopWRightDown origin moveTo board
                   | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "B" board)) &&
                     isBlackEnemy moveTo board = "," ++ 
                                                 convertIntToField origin ++ 
                                                 "-" ++ 
                                                 convertIntToField moveTo
                   | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "B" board)) &&
                     (isRightEdge moveTo ||
                     isLowerEdge moveTo) &&
                     isEmptyField moveTo board = "," ++
                                                 convertIntToField origin ++
                                                 "-" ++
                                                 convertIntToField moveTo
                   | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard origin moveTo "B" board)) &&
                     not (isRightEdge moveTo) &&
                     not (isLowerEdge moveTo) &&
                     isEmptyField moveTo board = "," ++
                                                 convertIntToField origin ++
                                                 "-" ++
                                                 convertIntToField moveTo ++
                                                 moveBishopWRightDown origin (moveTo + 10) board
                   | not (isRightEdge moveTo) &&
                     not (isLowerEdge moveTo) &&
                     isEmptyField moveTo board = moveBishopWRightDown origin (moveTo + 10) board
                   | otherwise = []

moveBishopBRightDown :: Int -> Int -> String -> String
moveBishopBRightDown origin moveTo board
                   | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "b" board)) &&
                     isWhiteEnemy moveTo board = "," ++ 
                                                 convertIntToField origin ++ 
                                                 "-" ++ 
                                                 convertIntToField moveTo
                   | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "b" board)) &&
                     (isRightEdge moveTo ||
                     isLowerEdge moveTo) &&
                     isEmptyField moveTo board = "," ++
                                                 convertIntToField origin ++
                                                 "-" ++
                                                 convertIntToField moveTo
                   | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard origin moveTo "b" board)) &&
                     not (isRightEdge moveTo) &&
                     not (isLowerEdge moveTo) &&
                     isEmptyField moveTo board = "," ++
                                                 convertIntToField origin ++
                                                 "-" ++
                                                 convertIntToField moveTo ++
                                                 moveBishopBRightDown origin (moveTo + 10) board
                   | not (isRightEdge moveTo) &&
                     not (isLowerEdge moveTo) &&
                     isEmptyField moveTo board = moveBishopBRightDown origin (moveTo + 10) board
                   | otherwise = []

doMoveBishopW :: Int -> String -> String
doMoveBishopW position board
            | not (elem position [0..7]) &&
              not (elem position [63.. 70]) &&
              not (elem position [9,18..54]) &&
              not (elem position [16,25..61]) = moveBishopWLeftUp position (position - 10) board ++
                                                moveBishopWRightUp position (position - 8) board ++
                                                moveBishopWLeftDown position (position + 8) board ++
                                                moveBishopWRightDown position (position + 10) board
            | position `elem` [9,18..54] = moveBishopWRightUp position (position - 8) board ++
                                           moveBishopWRightDown position (position + 10) board
            | position `elem` [1..6] = moveBishopWLeftDown position (position + 8) board ++
                                       moveBishopWRightDown position (position + 10) board
            | position `elem` [16,25..61] = moveBishopWLeftUp position (position - 10) board ++
                                            moveBishopWLeftDown position (position + 8) board
            | position `elem` [64..69] = moveBishopWLeftUp position (position - 10) board ++
                                         moveBishopWRightUp position (position - 8) board
            | position == 0 = moveBishopWRightDown position (position + 10) board
            | position == 7 = moveBishopWLeftDown position (position + 8) board
            | position == 63 = moveBishopWRightUp position (position - 8) board
            | position == 70 = moveBishopWLeftUp position (position - 10) board
            | otherwise = []

doMoveBishopB :: Int -> String -> String
doMoveBishopB position board
            | not (elem position [0..7]) &&
              not (elem position [63.. 70]) &&
              not (elem position [9,18..54]) &&
              not (elem position [16,25..61]) = moveBishopBLeftUp position (position - 10) board ++
                                                moveBishopBRightUp position (position - 8) board ++
                                                moveBishopBLeftDown position (position + 8) board ++
                                                moveBishopBRightDown position (position + 10) board
            | position `elem` [9,18..54] = moveBishopBRightUp position (position - 8) board ++
                                           moveBishopBRightDown position (position + 10) board
            | position `elem` [1..6] = moveBishopBLeftDown position (position + 8) board ++
                                       moveBishopBRightDown position (position + 10) board
            | position `elem` [16,25..61] = moveBishopBLeftUp position (position - 10) board ++
                                            moveBishopBLeftDown position (position + 8) board
            | position `elem` [64..69] = moveBishopBLeftUp position (position - 10) board ++
                                         moveBishopBRightUp position (position - 8) board
            | position == 0 = moveBishopBRightDown position (position + 10) board
            | position == 7 = moveBishopBLeftDown position (position + 8) board
            | position == 63 = moveBishopBRightUp position (position - 8) board
            | position == 70 = moveBishopBLeftUp position (position - 10) board
            | otherwise = []

-- ================================================================================
-- COMPUTE ALL QUEEN MOVES WITH CHECK DETECTION (everything's fine until now)
-- ================================================================================

doMoveQueenW :: Int -> String -> String
doMoveQueenW position board = (doMoveRookW position board) ++ (doMoveBishopW position board)

doMoveQueenB :: Int -> String -> String
doMoveQueenB position board = (doMoveRookB position board) ++ (doMoveBishopB position board)

-- ================================================================================
-- COMPUTE ALL KING MOVES WITH CHECK DETECTION (everything's fine until now)
-- ================================================================================

moveKingW :: Int -> Int -> String -> String
moveKingW origin moveTo board
        | not (checkedByBFigure moveTo (overrideOldBoard origin moveTo "K" board)) &&
          isBlackEnemy moveTo board = "," ++ 
                                      convertIntToField origin ++ 
                                      "-" ++ 
                                      convertIntToField moveTo
        | not (checkedByBFigure moveTo (overrideOldBoard origin moveTo "K" board)) &&
          isEmptyField moveTo board = "," ++ 
                                      convertIntToField origin ++ 
                                      "-" ++ 
                                      convertIntToField moveTo
        | otherwise = []

moveKingB :: Int -> Int -> String -> String
moveKingB origin moveTo board
        | not (checkedByWFigure moveTo (overrideOldBoard origin moveTo "k" board)) &&
          isWhiteEnemy moveTo board = "," ++ 
                                      convertIntToField origin ++ 
                                      "-" ++ 
                                      convertIntToField moveTo
        | not (checkedByWFigure moveTo (overrideOldBoard origin moveTo "k" board)) &&
          isEmptyField moveTo board = "," ++ 
                                      convertIntToField origin ++ 
                                      "-" ++ 
                                      convertIntToField moveTo
        | otherwise = []

doMoveKingW :: Int -> String -> String
doMoveKingW position board
          | not (elem position [0..7]) &&
            not (elem position [63.. 70]) &&
            not (elem position [9,18..54]) &&
            not (elem position [16,25..61]) = moveKingW position (position - 10) board ++
                                              moveKingW position (position - 9) board ++
                                              moveKingW position (position - 8) board ++
                                              moveKingW position (position - 1) board ++
                                              moveKingW position (position + 1) board ++
                                              moveKingW position (position + 8) board ++
                                              moveKingW position (position + 9) board ++
                                              moveKingW position (position + 10) board
          | position `elem` [9,18..54] = moveKingW position (position - 9) board ++
                                         moveKingW position (position - 8) board ++
                                         moveKingW position (position + 1) board ++
                                         moveKingW position (position + 9) board ++
                                         moveKingW position (position + 10) board
          | position `elem` [1..6] = moveKingW position (position - 1) board ++
                                     moveKingW position (position + 1) board ++
                                     moveKingW position (position + 8) board ++
                                     moveKingW position (position + 9) board ++
                                     moveKingW position (position + 10) board
          | position `elem` [16,25..61] = moveKingW position (position - 10) board ++
                                          moveKingW position (position - 9) board ++
                                          moveKingW position (position - 1) board ++
                                          moveKingW position (position + 8) board ++
                                          moveKingW position (position + 9) board
          | position `elem` [64..69] = moveKingW position (position - 10) board ++
                                       moveKingW position (position - 9) board ++
                                       moveKingW position (position - 8) board ++
                                       moveKingW position (position - 1) board ++
                                       moveKingW position (position + 1) board
          | position == 0 = moveKingW position (position + 1) board ++
                            moveKingW position (position + 9) board ++
                            moveKingW position (position + 10) board
          | position == 7 = moveKingW position (position - 1) board ++
                            moveKingW position (position + 8) board ++
                            moveKingW position (position + 9) board
          | position == 70 = moveKingW position (position - 10) board ++
                             moveKingW position (position - 9) board ++
                             moveKingW position (position - 1) board
          | position == 63 = moveKingW position (position - 9) board ++
                             moveKingW position (position - 8) board ++
                             moveKingW position (position + 1) board
          | otherwise = []

doMoveKingB :: Int -> String -> String
doMoveKingB position board
          | not (elem position [0..7]) &&
            not (elem position [63.. 70]) &&
            not (elem position [9,18..54]) &&
            not (elem position [16,25..61]) = moveKingB position (position - 10) board ++
                                              moveKingB position (position - 9) board ++
                                              moveKingB position (position - 8) board ++
                                              moveKingB position (position - 1) board ++
                                              moveKingB position (position + 1) board ++
                                              moveKingB position (position + 8) board ++
                                              moveKingB position (position + 9) board ++
                                              moveKingB position (position + 10) board
          | position `elem` [9,18..54] = moveKingB position (position - 9) board ++
                                         moveKingB position (position - 8) board ++
                                         moveKingB position (position + 1) board ++
                                         moveKingB position (position + 9) board ++
                                         moveKingB position (position + 10) board
          | position `elem` [1..6] = moveKingB position (position - 1) board ++
                                     moveKingB position (position + 1) board ++
                                     moveKingB position (position + 8) board ++
                                     moveKingB position (position + 9) board ++
                                     moveKingB position (position + 10) board
          | position `elem` [16,25..61] = moveKingB position (position - 10) board ++
                                          moveKingB position (position - 9) board ++
                                          moveKingB position (position - 1) board ++
                                          moveKingB position (position + 8) board ++
                                          moveKingB position (position + 9) board
          | position `elem` [64..69] = moveKingB position (position - 10) board ++
                                       moveKingB position (position - 9) board ++
                                       moveKingB position (position - 8) board ++
                                       moveKingB position (position - 1) board ++
                                       moveKingB position (position + 1) board
          | position == 0 = moveKingB position (position + 1) board ++
                            moveKingB position (position + 9) board ++
                            moveKingB position (position + 10) board
          | position == 7 = moveKingB position (position - 1) board ++
                            moveKingB position (position + 8) board ++
                            moveKingB position (position + 9) board
          | position == 70 = moveKingB position (position - 10) board ++
                             moveKingB position (position - 9) board ++
                             moveKingB position (position - 1) board
          | position == 63 = moveKingB position (position - 9) board ++
                             moveKingB position (position - 8) board ++
                             moveKingB position (position + 1) board
          | otherwise = []

-- ================================================================================
-- DROP MOVES WITH CHECK DETECTION (everything's fine until now)
-- ================================================================================

createCleanReserve :: String -> String
createCleanReserve board
                 | board == [] = []
                 | not ((head board) `elem` tail board) = [(head board)] ++ createCleanReserve (tail board)
                 | otherwise = createCleanReserve (tail board)

createReserve :: String -> String
createReserve board = createCleanReserve (init (drop 72 board))

dropPawnW :: Int -> String -> String
dropPawnW position board
        | (position `elem` [0..7]) ||
          (position `elem` [63..70]) = []
        | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard position position "P" board)) &&
          isEmptyField position board = "," ++
                                        "P" ++
                                        "-" ++
                                        convertIntToField position
        | otherwise = []

dropPawnB :: Int -> String -> String
dropPawnB position board
        | (position `elem` [0..7]) ||
          (position `elem` [63..70]) = []
        | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard position position "p" board)) &&
          isEmptyField position board = "," ++
                                        "p" ++
                                        "-" ++
                                        convertIntToField position
        | otherwise = []

dropRookW :: Int -> String -> String
dropRookW position board
        | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard position position "R" board)) &&
          isEmptyField position board = "," ++
                                        "R" ++
                                        "-" ++
                                        convertIntToField position
        | otherwise = []

dropRookB :: Int -> String -> String
dropRookB position board
        | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard position position "r" board)) &&
          isEmptyField position board = "," ++
                                        "r" ++
                                        "-" ++
                                        convertIntToField position
        | otherwise = []

dropKnightW :: Int -> String -> String
dropKnightW position board
          | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard position position "N" board)) &&
            isEmptyField position board = "," ++
                                          "N" ++
                                          "-" ++
                                          convertIntToField position
          | otherwise = []

dropKnightB :: Int -> String -> String
dropKnightB position board
          | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard position position "n" board)) &&
            isEmptyField position board = "," ++
                                          "n" ++
                                          "-" ++
                                          convertIntToField position
          | otherwise = []

dropBishopW :: Int -> String -> String
dropBishopW position board
          | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard position position "B" board)) &&
            isEmptyField position board = "," ++
                                          "B" ++
                                          "-" ++
                                          convertIntToField position
          | otherwise = []

dropBishopB :: Int -> String -> String
dropBishopB position board
          | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard position position "b" board)) &&
            isEmptyField position board = "," ++
                                          "b" ++
                                          "-" ++
                                          convertIntToField position
          | otherwise = []

dropQueenW :: Int -> String -> String
dropQueenW position board
         | not (checkedByBFigure (read (findOutKingWPosition 0 board)) (overrideOldBoard position position "Q" board)) &&
           isEmptyField position board = "," ++
                                         "Q" ++
                                         "-" ++
                                         convertIntToField position
         | otherwise = []

dropQueenB :: Int -> String -> String
dropQueenB position board
         | not (checkedByWFigure (read (findOutKingBPosition 0 board)) (overrideOldBoard position position "q" board)) &&
           isEmptyField position board = "," ++
                                         "q" ++
                                         "-" ++
                                         convertIntToField position
         | otherwise = []

selectCorrectFunctionToDropW :: Int -> String -> String -> String
selectCorrectFunctionToDropW position board figure
                           | figure == "P" = dropPawnW position board
                           | figure == "R" = dropRookW position board
                           | figure == "N" = dropKnightW position board
                           | figure == "B" = dropBishopW position board
                           | figure == "Q" = dropQueenW position board
                           | otherwise = []

selectCorrectFunctionToDropB :: Int -> String -> String -> String
selectCorrectFunctionToDropB position board figure
                           | figure == "p" = dropPawnB position board
                           | figure == "r" = dropRookB position board
                           | figure == "n" = dropKnightB position board
                           | figure == "b" = dropBishopB position board
                           | figure == "q" = dropQueenB position board
                           | otherwise = []

loopBoardW :: Int -> String -> String -> String
loopBoardW 71 _ _ = []
loopBoardW position board figure = selectCorrectFunctionToDropW position board figure ++
                                   loopBoardW (position + 1) board figure

loopBoardB :: Int -> String -> String -> String
loopBoardB 71 _ _ = []
loopBoardB position board figure = selectCorrectFunctionToDropB position board figure ++
                                   loopBoardB (position + 1) board figure

specifyFigureWDrop :: String -> String -> String
specifyFigureWDrop _ [] = []
specifyFigureWDrop board (x:xs) = loopBoardW 0 board [x] ++
                                  specifyFigureWDrop board xs

specifyFigureBDrop :: String -> String -> String
specifyFigureBDrop _ [] = []
specifyFigureBDrop board (x:xs) = loopBoardB 0 board [x] ++
                                  specifyFigureBDrop board xs
-- ================================================================================
-- GENERAL BOARD FUNCTIONS
-- ================================================================================

selectCorrectFunctionForFigureW :: Int -> String -> String -> String
selectCorrectFunctionForFigureW position board figure
                             | figure == "P" = doMovePawnW position board
                             | figure == "R" = doMoveRookW position board
                             | figure == "N" = doMoveKnightW position board
                             | figure == "B" = doMoveBishopW position board
                             | figure == "Q" = doMoveQueenW position board
                             | figure == "K" = doMoveKingW position board
                             | otherwise = []

selectCorrectFunctionForFigureB :: Int -> String -> String -> String
selectCorrectFunctionForFigureB position board figure
                             | figure == "p" = doMovePawnB position board
                             | figure == "r" = doMoveRookB position board
                             | figure == "n" = doMoveKnightB position board
                             | figure == "b" = doMoveBishopB position board
                             | figure == "q" = doMoveQueenB position board
                             | figure == "k" = doMoveKingB position board
                             | otherwise = []

specifyFigureW :: Int -> String -> String -> String
specifyFigureW 71 _ _ = []
specifyFigureW _ _ [] = []
specifyFigureW position board (x:xs) = selectCorrectFunctionForFigureW position board [x] ++ 
                                       specifyFigureW (position + 1) board xs

specifyFigureB :: Int -> String -> String -> String
specifyFigureB 71 _ _ = []
specifyFigureB _ _ [] = []
specifyFigureB position board (x:xs) = selectCorrectFunctionForFigureB position board [x] ++
                                       specifyFigureB (position + 1) board xs

colorDecider :: Int -> String -> String -> String
colorDecider position board1 board2
               | (last board1) == 'w' = specifyFigureW position board1 board2 ++
                                        specifyFigureWDrop board1 (createReserve board1)
               | otherwise = specifyFigureB position board1 board2 ++
                             specifyFigureBDrop board1 (createReserve board1)

prepareListMoves :: String -> String
prepareListMoves str
               | str == [] = []
               | otherwise = tail str
