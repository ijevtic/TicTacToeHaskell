import Control.Monad

listOfTuples :: [(Int,Char)]  
listOfTuples = do  
    n <- [1,2,3]  
    ch <- ['a','b']  
    return (n,ch)  


samoNeparni :: [(Int,Char)]
samoNeparni = do
    n <- [1,2,3]  
    ch <- ['a','b']  
    guard ((n `mod` 2)==1)
    return (n,ch)      



