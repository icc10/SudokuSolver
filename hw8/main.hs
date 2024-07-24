import System.IO
import Sudoku



main :: IO ()
main = do
  putStrLn "Enter a Sudoku puzzle row-by-row: "
  getInput 0 []

getInput :: Int -> [String] -> IO ()
getInput cnt (xs) = do
  if(cnt == 9)
    then do 
      Sudoku.show (Just (Sudoku xs))
      Sudoku.show (solve (Sudoku xs))
  else do
    input <- getLine
    getInput (cnt+1) ((xs) ++ [input])






      
 

