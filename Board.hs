module Board where
import Field
import Util
import Data.List (intercalate)
data Board a = Board [[a]] (Int, Int) | InvalidBoard


checkFinish :: Board Field -> [[(Int, Int)]] -> Bool
checkFinish (Board board (rows, cols)) [] = False
checkFinish (Board board (rows, cols)) (pattern: patterns) = 
	(((board !! x1) !! y1) == ((board !! x2) !! y2) && ((board !! x2) !! y2) == ((board !! x3) !! y3) && ((board !! x3) !! y3) /= EmptyF)
     || (checkFinish (Board board (rows, cols)) patterns)
	where
		(x1,y1) = pattern !! 0
		(x2,y2) = pattern !! 1
		(x3,y3) = pattern !! 2

checkFullBoard :: Board Field -> Bool
checkFullBoard (Board board (rows, cols)) = isListEmpty [field | row <- board, field <- row, field == EmptyF]

-- instance Show Field => Show (Board Field) where
--     show (Board board (rows, cols)) =
--         let formattedRows = map formatRow board
--             separator = ""
--             formattedBoard = intercalate separator formattedRows
--         in formattedBoard
--         where
--             formatRow row = '|' : intercalate "|" (map show row) ++ "|\n"
-- 		show InvalidBoard = "Invalid board"

instance Show a => Show (Board a) where
  show (Board board (rows, cols)) =
    let formattedRows = map formatRow board
        formattedBoard = intercalate "" formattedRows
		in formattedBoard
    where
      formatRow row = '|' : intercalate "|" (map show row) ++ "|\n"

  show InvalidBoard = "Invalid board"		

-- nepotrebno vrv   
printBoard :: Board Field -> IO ()
printBoard InvalidBoard = putStrLn "Invalid board"
printBoard (Board board (rows, cols)) = do
	mapM_ printRow board
	where
		printRow row = do
			putStr $ concat $ map (\x -> "|" ++ show x) row
			putStrLn "|"
			-- putStrLn ""