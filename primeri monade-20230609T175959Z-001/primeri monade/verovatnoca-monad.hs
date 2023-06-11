import Data.Ratio  
import Control.Monad
import Control.Applicative
import Data.List (all, nub)  
  
newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs   


{-

Applicative za listu
instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]  

-}

instance Applicative Prob where  
    pure x = Prob [(x,1%1)]
    Prob fs <*> Prob xs = Prob $ [(f x,p1*p2) | (f,p1) <- fs, (x,p2) <- xs]     

{-
test applicative Prob

> pure 5 :: Prob Int
Prob {getProb = [(5,1 % 1)]}

> Prob ([((+4),1%7),((*2),6%7)]) <*> Prob [(3,1%4),(4,3%4)]
Prob {getProb = [(7,1 % 28),(8,3 % 28),(6,3 % 14),(8,9 % 14)]}

-}


flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs  

instance Monad Prob where  
    return x = Prob [(x,1%1)]  
    m >>= f = flatten (fmap f m)  
    fail _ = Prob []  

{-

Da se dobije primer sa slajdova:
> Prob [('a',1%4),('c',3%4)] >>= (\x-> Prob [(x,1%2),(succ x,1%2)])
Prob {getProb = [('a',1 % 8),('b',1 % 8),('c',3 % 8),('d',3 % 8)]}


> Prob [('a',1%4),('c',3%4)] >>= (\x-> Prob [(x,1%2),(x,1%2)])
Prob {getProb = [('a',1 % 8),('a',1 % 8),('c',3 % 8),('c',3 % 8)]}
- bilo bi korisno da mozemo da skupimo verovatnoce za iste vrednosti

-}


skupiIste :: (Eq a, Num b)=> [(a,b)]->[(a,b)]
skupiIste xs = [(a,(sum [p1| (x,p1)<-xs,x==a]))| a <- elementi]
               where elementi = nub (fmap (\(x,y)->x) xs) 


skupiIsteProb :: (Eq a)=> Prob a -> Prob a
skupiIsteProb (Prob xs) = Prob $ [(a,(sum [p1| (x,p1)<-xs,x==a]))| a <- elementi]
                             where elementi = nub (fmap (\(x,y)->x) xs) 

{-
Primer korišćenja gore kreiranog tipa podatka

-}

data Novcic = Glava | Pismo deriving (Show, Eq)  
  
novcic :: Prob Novcic  
novcic = Prob [(Glava,1%2),(Pismo,1%2)]  
  
novcicF :: Prob Novcic  
novcicF = Prob [(Glava,1%10),(Pismo,9%10)]  

{-
Koja je verovatnoca da se u tri bacanja dobije pismo?
dva puta 
-}

triBacanja :: Prob Bool  
triBacanja = do  
    a <- novcic
    b <- novcic
    c <- novcicF  
    return (all (==Pismo) [a,b,c])



