import Control.Monad
import Data.Monoid



newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

-- pakovanje liste u diferencijalnu listu 

toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  

-- raspakivanje liste (iz diferencijalne liste u listu)
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []  

-- monoid 

instance Semigroup (DiffList a) where
   (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))  # ili f . g


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

ispisiBroj :: Int -> Writer (DiffList String) Int  
ispisiBroj x = writer (x, toDiffList ["Izvucen broj: " ++ show x])  
  
mnozenjeSaIspisom :: Writer (DiffList String) Int  
mnozenjeSaIspisom = do  
    a <- ispisiBroj 3  
    b <- ispisiBroj 5   
    tell (toDiffList ["mnozenje dva broja"])
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

