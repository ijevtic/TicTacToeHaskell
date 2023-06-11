import Control.Monad
import Data.Monoid

newtype MojWriter w a = MojWriter { runWriter :: (a, w) }  deriving Show

instance (Monoid w) => Functor (MojWriter w) where  
    fmap f (MojWriter (x,v1)) = MojWriter (f x, v1) 

instance (Monoid w) => Applicative (MojWriter w) where  
    pure x = MojWriter (x, mempty)  
    MojWriter (f,v) <*> MojWriter (x,v1)  = MojWriter (f x, v `mappend` v1) 
 
instance (Monoid w) => Monad (MojWriter w) where  
    return x = MojWriter (x, mempty)  
    (MojWriter (x,v)) >>= f = let (MojWriter (y, v')) = f x in MojWriter (y, v `mappend` v') 
 
newtype Sabirak = Sabirak {vrednost::Int} deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup Sabirak where
   Sabirak s1 <> Sabirak s2 = Sabirak (s1+s2)
  

instance Monoid Sabirak where
    mempty = Sabirak 0


{--
mojtell :: [String] ->   MojWriter [String] () 
mojtell lista = MojWriter ((),lista)
--}

mojtell :: Monoid m => m ->   MojWriter m () 
mojtell m = MojWriter ((),m)
  
  
ispisiBroj :: Int -> MojWriter [String] Int  
ispisiBroj x = MojWriter (x, ["Izvucen broj: " ++ show x])  
  
mnozenjeSaIspisom :: MojWriter [String] Int  
mnozenjeSaIspisom = do  
    a <- ispisiBroj 3  
    b <- ispisiBroj 5   
    mojtell ["mnozenje dva broja"]
    return (a*b)
    
    
saberiBrojSab :: Int -> MojWriter Sabirak Int
saberiBrojSab sab = MojWriter (1, Sabirak sab)

saberi :: MojWriter Sabirak Int
saberi = do
    a <- saberiBrojSab 5
    b <- saberiBrojSab 10    
    return (a+b)
    
    
newtype Brojac = Brojac {br::Int} deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup Brojac where
   Brojac b1 <> Brojac b2 = Brojac (b1+b2)
  

instance Monoid Brojac where
    mempty = Brojac 0
    
opBrojac:: Int -> MojWriter Brojac Int
opBrojac op = MojWriter (op, Brojac 1)

izracunajBr :: MojWriter Brojac Int

izracunajBr = do
    a <- opBrojac 3
    b <- opBrojac 10
    c <- opBrojac 17    
    return (a*b+c)

-- Diferencijalna lista

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

-- pakovanje liste u diferencijalnu listu 

toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  

-- raspakivanje liste (iz diferencijalne liste u listu)
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []  

-- monoid 

instance Semigroup (DiffList a) where
   (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))  -- ili f . g

instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    

{--

> pom = toDiffList [1,2,3,4,5]

> getDiffList pom [7,8,9]
[1,2,3,4,5,7,8,9]

> fromDiffList pom
[1,2,3,4,5]

> pom1 = toDiffList [10,11,12]

> pom2 = pom `mappend` pom1

> fromDiffList pom2

--}

ispisiBrojDl :: Int -> MojWriter (DiffList String) Int  
ispisiBrojDl x = MojWriter (x, toDiffList ["Izvucen broj: " ++ show x])  
  
mnozenjeSaIspisomDl :: MojWriter (DiffList String) Int  
mnozenjeSaIspisomDl = do  
    a <- ispisiBrojDl 3  
    b <- ispisiBrojDl 5   
    mojtell (toDiffList ["mnozenje dva broja"])
    return (a*b)


{--
> pom = runWriter mnozenjeSaIspisom 
> :t pom
pom :: (Int, DiffList String)
> fst pom
15
> fromDiffList $ snd pom
["Izvucen broj: 3","Izvucen broj: 5","mnozenje dva broja"]


--}

   


    
    
