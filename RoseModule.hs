module RoseModule where

data Rose a = Node a [Rose a] deriving (Show)

-- a) size
size :: Rose a -> Int
size (Node _ children) = 1 + sum (map size children)

-- b) height
height :: Rose a -> Int
height (Node _ []) = 0
height (Node _ children) = 1 + maximum (map height children)

-- b) leavesCount
leavesCount :: Rose a -> Int
leavesCount (Node _ []) = 1
leavesCount (Node _ children) = sum (map leavesCount children)

-- b) leaves
leaves :: Rose a -> [a]
leaves (Node value []) = [value]
leaves (Node _ children) = concatMap leaves children

-- c) elemsOnDepth
elemsOnDepth :: Int -> Rose a -> [a]
elemsOnDepth 0 (Node value _) = [value]
elemsOnDepth _ (Node _ []) = []
elemsOnDepth depth (Node _ children) = concatMap (elemsOnDepth (depth - 1)) children

-- d) Functor instance
instance Functor Rose where
  fmap f (Node value children) = Node (f value) (map (fmap f) children)

-- e) foldRose
foldRose :: (a -> b -> b) -> b -> Rose a -> b
foldRose f acc (Node value children) = foldl (foldRose f) (f value acc) children
