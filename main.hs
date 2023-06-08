import RoseModule

data Player = Player1 | Player2 deriving (Show)
data GameState a = GameState Player (Rose a) deriving (Show)
data Move a = Move Player a deriving (Show)

newtype GameStateOp a = GameStateOp { runGameStateOp :: GameState a -> (a, GameState a) }

instance Functor GameStateOp where
  fmap f (GameStateOp op) = GameStateOp (\s -> let (x, s') = op s in (f x, s'))

instance Applicative GameStateOp where
  pure x = GameStateOp (\s -> (x, s))
  GameStateOp f <*> GameStateOp x = GameStateOp (\s -> let (g, s') = f s; (y, s'') = x s' in (g y, s''))

instance Monad GameStateOp where
  return = pure
  GameStateOp x >>= f = GameStateOp (\s -> let (y, s') = x s; GameStateOp op = f y in op s')

-- newtype GameStateOpHistory a = GameStateOpHistory { runGameStateOpHistory :: [GameState a] -> ([GameState a], a) }

-- instance Functor GameStateOpHistory where
--   fmap f (GameStateOpHistory op) = GameStateOpHistory (\history -> let (newHistory, x) = op history in (newHistory, f x))

-- instance Applicative GameStateOpHistory where
--   pure x = GameStateOpHistory (\history -> (history, x))
--   GameStateOpHistory f <*> GameStateOpHistory x = GameStateOpHistory (\history -> let (history', g) = f history; (history'', y) = x history' in (history'', g y))

-- instance Monad GameStateOpHistory where
--   return = pure
--   GameStateOpHistory x >>= f = GameStateOpHistory (\history -> let (history', y) = x history; GameStateOpHistory op = f y in op history')


-- main
main = do
  putStrLn "Hello World!"