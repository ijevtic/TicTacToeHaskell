module GameStateHistory where

import Control.Monad.State

-- s je tabla
newtype GameStateHistory s a = GameStateHistory { runGameStateHistory :: s -> (a, [s]) }

--
instance Functor (GameStateHistory s) where
  fmap f (GameStateHistory g) = GameStateHistory $ \tabla -> 
    let (a, lista') = g tabla
    in (f a, lista')

-- f je funkcija u funktoru, g je vrednost u funktoru
instance Applicative (GameStateHistory s) where
  pure x = GameStateHistory $ \tabla -> (x, [tabla])
  (GameStateHistory f) <*> (GameStateHistory g) =
    GameStateHistory $ \tabla ->
      let (f1, lista') = f tabla
          (x:xs) = lista'
          (a2, t2) = g x
      in (f1 a2, t2 ++ xs)

instance Monad (GameStateHistory s) where
  return x = GameStateHistory (\s -> (x, [s]))
  
  (GameStateHistory h) >>= f = GameStateHistory (\s ->
    let (a, lista') = h s
        (GameStateHistory g) = f a
        (b, lista'') = g (head lista')
        -- final_list = if (head updated_list1) == (head updated_list) then updated_list else updated_list1 ++ updated_list
        final_list = lista'' ++ lista'
    in (b, final_list))