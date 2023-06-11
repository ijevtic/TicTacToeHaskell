module GameStateHistory where

import Control.Monad.State

-- s je tabla
newtype GameStateHistory s a = GameStateHistory { runGameStateHistory :: s -> (a, s) }

--
instance Functor (GameStateHistory s) where
  fmap f (GameStateHistory g) = GameStateHistory $ \tabla -> 
    let (a, tabla') = g tabla
    in (f a, tabla')

-- f je funkcija u funktoru, g je vrednost u funktoru
instance Applicative (GameStateHistory s) where
  pure x = GameStateHistory $ \tabla -> (x, tabla)
  (GameStateHistory f) <*> (GameStateHistory g) =
    GameStateHistory $ \tabla ->
      let (f1, t1) = f tabla
          (a2, t2) = g t1
      in (f1 a2, t2)

instance Monad (GameStateHistory s) where
  return = pure
  (GameStateHistory f) >>= g = GameStateHistory $ \tabla ->
    let (a, tabla') = f tabla
        (GameStateHistory h) = g a
    in h tabla'
