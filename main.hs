import Control.Monad.State
import RoseModule
import GameStateOp
import GameStateHistory
import Field

data Board a = Board [[a]] (Int, Int) deriving (Show)

data Player = Player1 | Player2 deriving (Eq, Show)
data GameState a = GameState Player (Board a) deriving (Show)
data Move = Move Player (Int, Int) deriving (Show)

startBoard :: Board Field
startBoard = Board [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]] (3, 3)

startState :: GameState Field
startState = GameState Player1 startBoard

printBoard :: Board Field -> IO ()
printBoard (Board board (rows, cols)) = do
	mapM_ printRow board
	where
		printRow row = do
			putStr $ concat $ map (\x -> "|" ++ show x) row
			putStrLn "|"
			-- putStrLn ""

checkValidMove :: GameState Field -> Move -> Bool
checkValidMove (GameState player (Board board (rows, cols))) (Move movePlayer (row, col)) = 
	(row >= 0) && (row < rows) && (col >= 0) && (col < cols) && ((board !! row) !! col == Empty) && player == movePlayer

returnAllValidMoves :: GameState Field -> [Move]
returnAllValidMoves (GameState player (Board board (rows, cols))) = 
	[Move player (row, col) | row <- [0..(rows - 1)], col <- [0..(cols - 1)], checkValidMove gameState (Move player (row, col))]
	where
		gameState = GameState player (Board board (rows, cols))

-- main
main = do
	printBoard startBoard
	-- [putStrLn move | move <- (returnAllValidMoves startState)]