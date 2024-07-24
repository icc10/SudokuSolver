module Sudoku
  (Sudoku(..), solve, Sudoku.show)
where

import System.IO
import qualified Data.Maybe


data Sudoku = Sudoku [[Char]]

instance Show Sudoku where
show ::  Maybe Sudoku -> IO ()
show Nothing = do
  putStrLn ""
show (Just(Sudoku xs)) = do
  putStrLn ("+===+===+===+===+===+===+===+===+===+")
  printCols 0 (xs !! 0)
  putStrLn ("+---+---+---+---+---+---+---+---+---+")
  printCols 0 (xs !! 1)
  putStrLn ("+---+---+---+---+---+---+---+---+---+")
  printCols 0 (xs !! 2)
  putStrLn ("+===+===+===+===+===+===+===+===+===+")
  printCols 0 (xs !! 3)
  putStrLn ("+---+---+---+---+---+---+---+---+---+")
  printCols 0 (xs !! 4)
  putStrLn ("+---+---+---+---+---+---+---+---+---+")
  printCols 0 (xs !! 5)
  putStrLn ("+===+===+===+===+===+===+===+===+===+")
  printCols 0 (xs !! 6)
  putStrLn ("+---+---+---+---+---+---+---+---+---+")
  printCols 0 (xs !! 7)
  putStrLn ("+---+---+---+---+---+---+---+---+---+")
  printCols 0 (xs !! 8)
  putStrLn ("+===+===+===+===+===+===+===+===+===+")


printCols:: Int -> String -> IO ()
printCols _ [] = do
  putStrLn ""
printCols cnt (x:xs) = do
  if(cnt == 0)
    then do
      putStr ("| " ++ [x] ++ " : ")
      hFlush stdout
      printCols (cnt +1) (xs)
    else if(cnt == 1 || cnt == 3 || cnt == 4 || cnt == 6 || cnt == 7)
      then do
        putStr ([x] ++ " : ")
        hFlush stdout
        printCols (cnt +1) (xs)
      else do
        putStr ([x] ++ " | ")
        hFlush stdout
        printCols (cnt +1) (xs)

solve :: Sudoku -> Maybe Sudoku
solve (Sudoku []) = Nothing
solve (Sudoku xs) = helpSolve (Sudoku xs)


helpSolve :: Sudoku -> Maybe Sudoku
helpSolve (Sudoku xs)
                | firstEmptySpot == (-1, -1) = (Just (Sudoku xs))
                | firstEmptySpot /= (-1, -1) && length possibleNum == 0 = Nothing
                | firstEmptySpot /= (-1, -1) && length possibleNum /= 0 = tryEveryNum firstEmptySpot possibleNum (Sudoku xs)
                where
                  firstEmptySpot = findEmpty 0 (Sudoku xs)
                  possibleNum = simplify (different (different (different (Prelude.show [1..9]) (getRow firstEmptySpot xs)) (getColumn (Sudoku xs) (snd firstEmptySpot) 0)) (getBlock firstEmptySpot (Sudoku xs)))


simplify :: String -> String
simplify [] = []
simplify (x:xs) 
                  | (fromEnum x - 48) < 10 && (fromEnum x - 48) > 0 = [x] ++ simplify xs
                  | otherwise = simplify xs
  

tryEveryNum :: (Int, Int) -> String -> Sudoku -> Maybe Sudoku
tryEveryNum _ [] _ = Nothing
tryEveryNum (emptyRow, emptyCol) (y:ys) (Sudoku xs)
                | Data.Maybe.isJust result = result
                | Data.Maybe.isNothing result = tryEveryNum (emptyRow, emptyCol) ys (Sudoku xs)
                where result = helpSolve (Sudoku (updateSudoku (emptyRow, emptyCol) (0,0) y xs))

different :: String -> String -> String
different [] [] = []
different (x:xs) [] = (x:xs)
different [] (y:ys) = []
different (x:xs) (y:ys)
                        | myElem x (y:ys) == False = x : different (xs) (y:ys)
                        | otherwise = different (xs) (y:ys)

findEmpty :: Int -> Sudoku -> (Int, Int)
findEmpty 9 _ = (-1,-1)
findEmpty row (Sudoku xs)
                | foundCol == -1 = findEmpty (row+1) (Sudoku xs)
                | otherwise = (row, foundCol)
                where foundCol =  findEmptyCol (xs !! row) 0

findEmptyCol :: String -> Int -> Int
findEmptyCol [] _ = -1
findEmptyCol (x:xs) curCol
                | x == ' ' = curCol
                | otherwise = findEmptyCol xs (curCol + 1)

getRow :: (Int, Int) -> [String] -> String
getRow (row, col) xs = xs !! row

myElem :: Char -> String -> Bool
myElem y [] = False
myElem y (x:xs)
                | y == x = True
                | otherwise = myElem y xs

checkCol :: (Int, Int) -> Int -> Sudoku -> Bool
checkCol (row, col) cnt (Sudoku xs)
                | myElem (head (Prelude.show cnt)) colOfNum == False = True
                | otherwise = False
                where colOfNum = getColumn (Sudoku xs) col 0
getColumn :: Sudoku -> Int -> Int -> String
getColumn (Sudoku xs) col 9 = ""
getColumn (Sudoku xs) col rowCnt = getColNum (xs !! rowCnt) col 0 ++ getColumn (Sudoku xs) col (rowCnt+1)

getColNum :: [Char] -> Int -> Int -> [Char]
getColNum (x:[]) col cnt
                | cnt /= col = []
                | otherwise = [x]
getColNum (x:xs) col cnt
                | cnt /= col = getColNum xs col (cnt+1)
                | otherwise = [x]

getBlock :: (Int, Int) -> Sudoku -> String
getBlock (row, col) (Sudoku xs)
                | row < 3 && col < 3 = take 3 (xs !! 0) ++ take 3 (xs !! 1) ++ take 3 (xs !! 2)
                | row < 6 && col < 3 = take 3 (xs !! 3) ++ take 3 (xs !! 4) ++ take 3 (xs !! 5)
                | row < 9 && col < 3 = take 3 (xs !! 6) ++ take 3 (xs !! 7) ++ take 3 (xs !! 8)
                | row < 3 && col < 6 = take 3 (drop 3 (xs !! 0)) ++ take 3 (drop 3 (xs !! 1)) ++ take 3 (drop 3 (xs !! 2))
                | row < 6 && col < 6 = take 3 (drop 3 (xs !! 3)) ++ take 3 (drop 3 (xs !! 4)) ++ take 3 (drop 3 (xs !! 5))
                | row < 9 && col < 6 = take 3 (drop 3 (xs !! 6)) ++ take 3 (drop 3 (xs !! 7)) ++ take 3 (drop 3 (xs !! 8))
                | row < 3 && col < 9 = take 3 (drop 6 (xs !! 0)) ++ take 3 (drop 6 (xs !! 1)) ++ take 3 (drop 6 (xs !! 2))
                | row < 6 && col < 9 = take 3 (drop 6 (xs !! 3)) ++ take 3 (drop 6 (xs !! 4)) ++ take 3 (drop 6 (xs !! 5))
                | row < 9 && col < 9 = take 3 (drop 6 (xs !! 6)) ++ take 3 (drop 6 (xs !! 7)) ++ take 3 (drop 6 (xs !! 8))


updateSudoku :: (Int, Int) -> (Int, Int) -> Char -> [String] -> [String]
updateSudoku _ (9,_) _ _ = []
updateSudoku _ (_,9) _ _ = []
updateSudoku (rowLoc, colLoc) (curRow, curCol) cnt xs
                | curRow /= rowLoc = [(xs !! curRow)] ++ updateSudoku (rowLoc, colLoc) ((curRow + 1), curCol) cnt xs
                | otherwise = [copyCol (rowLoc, colLoc) (curRow, curCol) cnt (xs !! curRow)] ++ updateSudoku (rowLoc, colLoc) ((curRow + 1), 0) cnt xs


copyCol :: (Int, Int) -> (Int, Int) -> Char -> String -> String
copyCol _ (9,_) _ _ = []
copyCol _ (_,9) _ _ = []
copyCol (rowLoc, colLoc) (curRow, curCol) cnt xs
                | curCol /= colLoc = (getColNum xs curCol 0) ++ copyCol (rowLoc, colLoc) (curRow, (curCol + 1)) cnt xs
                | otherwise = [cnt] ++ copyCol (rowLoc, colLoc) (curRow, (curCol + 1)) cnt xs

