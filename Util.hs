module Util where

isListEmpty :: [a] -> Bool
isListEmpty [] = True
isListEmpty _ = False

finishPatterns :: [[(Int, Int)]]
finishPatterns = [[(0, 0), (0, 1), (0, 2)], 
	[(1, 0), (1, 1), (1, 2)], 
	[(2, 0), (2, 1), (2, 2)], 
	[(0, 0), (1, 0), (2, 0)], 
	[(0, 1), (1, 1), (2, 1)], 
	[(0, 2), (1, 2), (2, 2)], 
	[(0, 0), (1, 1), (2, 2)], 
	[(0, 2), (1, 1), (2, 0)]]