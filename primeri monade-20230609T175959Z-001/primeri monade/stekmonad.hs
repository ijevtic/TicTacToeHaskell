import Control.Monad.State  
  
type Stack = [Int] 

pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)  

stackManip :: State Stack Int  
stackManip = do               
    pop
    pop
    push 3
    pop
    pop	

{-  
>runState (pop >> pop >> pop) [1,2,3,4,5]
(3,[4,5])
> runState (pop >>= \x-> push x) [1,2,3,4,5]
((),[1,2,3,4,5])
-}

stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3
	    push 10

moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip  
    if a == 100  
        then stackStuff  
        else return () 

stackyStack :: State Stack ()  
stackyStack = do  
    stackNow <- get  
    if stackNow == [1,2,3]  
        then put [8,3,1]  
        else put [9,2,1]  
 	  
            
