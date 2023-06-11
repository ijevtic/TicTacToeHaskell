module GameStateOp where

import Control.Monad.State

-- s je tabla
newtype GameStateOp s a = GameStateOp { runGameStateOp :: s -> (a, s) }

--
instance Functor (GameStateOp s) where
  fmap f (GameStateOp g) = GameStateOp $ \tabla -> 
    let (a, tabla') = g tabla
    in (f a, tabla')

-- f je funkcija u funktoru, g je vrednost u funktoru
instance Applicative (GameStateOp s) where
  pure x = GameStateOp $ \tabla -> (x, tabla)
  (GameStateOp f) <*> (GameStateOp g) =
    GameStateOp $ \tabla ->
      let (f1, t1) = f tabla
          (a2, t2) = g t1
      in (f1 a2, t2)

instance Monad (GameStateOp s) where
  return = pure
  (GameStateOp f) >>= g = GameStateOp $ \tabla ->
    let (a, tabla') = f tabla
        (GameStateOp h) = g a
    in h tabla'
