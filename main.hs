import Control.Monad.State
import Text.Parsec
import Data.Char (ord)
import RoseModule
import GameStateOp
import GameStateHistory
import Field
import Player
import Move
import Board
import Util

data GameState a = GameState Player (Board a) | Invalid

instance Show Field => Show (GameState Field) where
	show (GameState player board) = show player ++ "\n" ++ show board


instance Eq Field => Eq (GameState Field) where
	Invalid == Invalid = True
	-- (GameState player1 board1) == (GameState player2 board2) = player1 == player2 && board1 == board2
	_ == _ = False

startRow = [EmptyF, EmptyF, EmptyF]

makeBoard :: Int -> Int -> [[Field]] -> Board Field
makeBoard rowCnt colCnt rows = Board rows (rowCnt, colCnt)

makeState :: Player -> Board Field -> GameState Field
makeState player board = GameState player board

startBoard :: Board Field
startBoard = makeBoard 3 3 [startRow, startRow, startRow]
-- startBoard = Board [[EmptyF, EmptyF, EmptyF], [EmptyF, EmptyF, EmptyF], [EmptyF, EmptyF, EmptyF]] (3, 3)

startState :: GameState Field
startState = GameState Player1 startBoard

getBoard :: GameState Field -> Board Field
getBoard Invalid = InvalidBoard
getBoard (GameState _ board) = board

getPlayer :: GameState Field -> Player
getPlayer (GameState player _) = player

checkValidMove :: GameState Field -> Move -> Bool
checkValidMove (GameState player (Board board (rows, cols))) (Move movePlayer (row, col)) = 
	(row >= 0) && (row < rows) && (col >= 0) && (col < cols) && ((board !! row) !! col == EmptyF) && player == movePlayer

isGameOver :: GameState Field -> Bool
isGameOver Invalid = True
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

makeMove :: GameState Field -> Move -> GameState Field
makeMove (GameState player (Board board (rows, cols))) move = 
	if checkValidMove (GameState player (Board board (rows, cols))) move == False then Invalid
		else GameState (nextPlayer player) (Board newBoard (rows, cols))
	where
		newBoard = [(applyRow (board !! numRow) numRow move) | numRow <- [0..(rows - 1)]]


makeRoseTree :: GameState Field -> Rose (GameState Field)
makeRoseTree gameState = Node gameState (map makeRoseTree (map (makeMove gameState) (returnAllValidMoves gameState)))

printRoseTree :: Rose (GameState Field) -> Int -> IO ()
printRoseTree (Node gameState children) depth = do
	putStrLn $ "depth: " ++ show depth ++ "\n"
	putStrLn $ show gameState
	mapM_ (\child -> printRoseTree child (depth + 1)) children


applyMove :: (Int, Int) -> GameStateOp (GameState Field) Bool
applyMove (x, y) = GameStateOp (\gameState -> let
	newGameState = makeMove gameState (Move (getPlayer gameState) (x, y))
	in
		(isGameOver newGameState, newGameState))

applyMove_ :: Move -> GameStateOp (GameState Field) Bool
applyMove_ move = GameStateOp (\gameState -> let
	newGameState = if gameState == Invalid then Invalid else makeMove gameState move
	in
		(isGameOver newGameState, newGameState))


applyMoveH :: (Int, Int) -> GameStateHistory (GameState Field) Bool
applyMoveH (x, y) = GameStateHistory (\gameState -> let
	newGameState = makeMove gameState (Move (getPlayer gameState) (x, y))
	in
		(isGameOver newGameState, [newGameState]))

initialize :: GameStateHistory (GameState Field) Bool
initialize = (GameStateHistory (\gameState -> (isGameOver gameState, [gameState]))) 

-- test
testBoard :: Board Field
testBoard = Board [[X, O, O], [X, X, O], [EmptyF, X, EmptyF]] (3, 3)
testState :: GameState Field
testState = GameState Player2 testBoard

testApplyMoves :: GameStateOp (GameState Field) Bool
testApplyMoves = do
	applyMove (0, 0)
	applyMove (0, 1)
	applyMove (0, 2)
	applyMove (1, 0)
	applyMove (1, 1)
	applyMove (1, 2)
	applyMove (2, 2)
	applyMove (2, 0)
	-- applyMove (0, 2)

testApplyMovesH = do
	applyMoveH (0, 0)
	applyMoveH (0, 1)
	applyMoveH (0, 2)
	applyMoveH (1, 0)
	applyMoveH (1, 1)
	applyMoveH (1, 2)
	applyMoveH (2, 2)
	applyMoveH (2, 0)
	-- applyMoveH (0, 2)


readBoard :: Parsec String (GameState Field) (GameState Field)
readBoard = do
	row1 <- readRow
	row2 <- readRow
	row3 <- readRow
	-- row2 <- [EmptyF, EmptyF, EmptyF]
	-- row3 <- [EmptyF, EmptyF, EmptyF]
	-- row2 <- readRow
	-- row3 <- readRow
	setState (makeState Player1 (makeBoard 3 3 [row1, row2, row3]))
	getState

readField :: Parsec String (GameState Field) Field
readField = do
	spaces
	val <- anyChar
	spaces
	char '|'
	case val of
		'X' -> return X
		'O' -> return O
		'_' -> return EmptyF
		_ -> fail "invalid field"

breaker :: Parsec String (GameState Field) ()
breaker = spaces >> char '|' >> spaces

newLine :: Parsec String (GameState Field) ()
newLine = spaces >> char '\n' >> spaces

readRow :: Parsec String (GameState Field) [Field]
readRow = do
		spaces
		char '|'
		f1 <- readField
		f2 <- readField
		f3 <- readField
		-- fields <- between breaker breaker (sepBy readField breaker)
		-- newLine
		-- return fields
		return [f1, f2, f3]

readPlayer :: Parsec String (GameState Field) Player
readPlayer = do
	spaces
	p <- anyChar
	if p == 'X' then return Player1 else return Player2

readMove :: Parsec String (GameState Field) Move
readMove = do
				spaces
				player <- readPlayer
				spaces
				char '('
				row <- digit
				char ','
				col <- digit
				char ')'
				spaces
				return ((Move player (ord row - 48, ord col - 48)))

readMoves :: Parsec String (GameState Field) [Move]
readMoves = do
				moves <- many readMove
				return moves



applyMoves :: [GameStateOp (GameState Field) Bool] -> GameState Field -> (Bool, GameState Field) 
applyMoves [] state = (isGameOver state, state)
applyMoves (x:xs) state = runGameStateOp (foldl (\acc elem -> acc >> elem) x xs) state

getData :: Parsec String (GameState Field) (GameState Field, [Move])
getData = do
	readBoard
	moves <- readMoves
	state <- getState
	return (state, moves)

main = do
	contents <- readFile "file.txt"
	case runParser getData startState "file.txt" contents of
		Left err -> print err
		Right (state, moves) -> do
			putStrLn "Start board"
			printBoard $ getBoard state
			putStrLn "Moves"
			printMoves moves
			let
				(isOver, endState) = (applyMoves (map (\move -> applyMove_ move) moves) state)
			putStrLn "End board"
			printBoard $ getBoard endState
			putStrLn $ show isOver
			-- putStrLn $ show $ isGameOver state
			-- printBoard $ getBoard state
			-- printMoves moves
