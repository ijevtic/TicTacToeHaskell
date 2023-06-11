import Control.Monad.State
import RoseModule
import GameStateOp
import GameStateHistory
import Field
import Player
import Move
import Board
import Util

data GameState a = GameState Player (Board a)

instance Show Field => Show (GameState Field) where
	show (GameState player board) = show player ++ "\n" ++ show board

startBoard :: Board Field
startBoard = Board [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]] (3, 3)

startState :: GameState Field
startState = GameState Player1 startBoard

getBoard :: GameState Field -> Board Field
getBoard (GameState _ board) = board

checkValidMove :: GameState Field -> Move -> Bool
checkValidMove (GameState player (Board board (rows, cols))) (Move movePlayer (row, col)) = 
	(row >= 0) && (row < rows) && (col >= 0) && (col < cols) && ((board !! row) !! col == Empty) && player == movePlayer

isGameOver :: GameState Field -> Bool
isGameOver (GameState _ board) = checkFinish board finishPatterns || checkFullBoard board

returnAllValidMoves :: GameState Field -> [Move]
returnAllValidMoves (GameState player (Board board (rows, cols))) = 
	if isGameOver (GameState player (Board board (rows, cols))) then [] else
	[Move player (row, col) | row <- [0..(rows - 1)], col <- [0..(cols - 1)], checkValidMove gameState (Move player (row, col))]
	where
		gameState = GameState player (Board board (rows, cols))

applyRow :: [Field] -> Int -> Move -> [Field]
applyRow row rowNumber (Move movePlayer (rowInd, colInd)) = 
	[if (rowNumber == rowInd) && (colInd == colNum) then (if movePlayer == Player1 then X else O) else row !! colNum | colNum <- [0..((length row) - 1)]]

applyMove :: GameState Field -> Move -> GameState Field
applyMove (GameState player (Board board (rows, cols))) move = 
	GameState (nextPlayer player) (Board newBoard (rows, cols))
	where
		newBoard = [(applyRow (board !! numRow) numRow move) | numRow <- [0..(rows - 1)]]


makeRoseTree :: GameState Field -> Rose (GameState Field)
makeRoseTree gameState = Node gameState (map makeRoseTree (map (applyMove gameState) (returnAllValidMoves gameState)))

printRoseTree :: Rose (GameState Field) -> Int -> IO ()
printRoseTree (Node gameState children) depth = do
	putStrLn $ "depth: " ++ show depth ++ "\n"
	putStrLn $ show gameState
	mapM_ (\child -> printRoseTree child (depth + 1)) children

-- test
testBoard :: Board Field
testBoard = Board [[X, O, O], [X, X, O], [Empty, X, Empty]] (3, 3)
testState :: GameState Field
testState = GameState Player2 testBoard


-- main
main = do
	printBoard testBoard
	putStrLn "---"
	--get board from state
	
	-- printRoseTree (makeRoseTree testState) 0
	-- printRoseTree (makeRoseTree startState) 0

	putStrLn $ "num: " ++ show (leavesCount (makeRoseTree startState))


	-- printBoard $ getBoard (applyMove testState (Move Player1 (0, 0)))
	-- printMoves [move | move <- (returnAllValidMoves testState)]
	-- putStrLn $ show $ isGameOver testState
	-- [putStrLn move | move <- (returnAllValidMoves startState)]