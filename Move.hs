module Move where
import Player

data Move = Move Player (Int, Int)

instance Show Move where
    show (Move player (row, col)) = show player ++ " " ++ show row ++ " " ++ show col

printMoves :: [Move] -> IO ()
printMoves [] = return ()
printMoves (x:xs) = do
	putStrLn $ show x
	printMoves xs