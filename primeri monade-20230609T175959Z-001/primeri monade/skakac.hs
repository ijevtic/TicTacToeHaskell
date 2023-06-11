import Control.Monad


type SkakacPozicija = (Char,Int)

plus2 x = succ $ succ x
minus2 x = pred $ pred x

pomeriSkakaca :: SkakacPozicija -> [SkakacPozicija]  
pomeriSkakaca (c,r) = do  
    (c',r') <- [(plus2 c,r-1),(plus2 c,r+1),(minus2 c,r-1),(minus2 c,r+1)  
               ,(succ c,r-2),(succ c,r+2),(pred c,r-2),(pred c,r+2)  
               ]  
    guard (c' `elem` "abcdefgh" && r' `elem` [1..8])  
    return (c',r')  
 
uTriPoteza :: SkakacPozicija -> [SkakacPozicija]  
-- uTriPoteza pocetna = do   
--    prva <- pomeriSkakaca pocetna  
--    druga <- pomeriSkakaca prva  
--    pomeriSkakaca druga  


uTriPoteza pocetna = return pocetna >>= pomeriSkakaca >>= pomeriSkakaca >>= pomeriSkakaca

mozeUTri :: SkakacPozicija -> SkakacPozicija -> Bool

mozeUTri pocetna krajnja = elem krajnja $ uTriPoteza pocetna


