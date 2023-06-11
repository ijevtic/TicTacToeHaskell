module Field where

data Field = X | O | Empty deriving (Eq)

instance Show Field where
	show X = "X"
	show O = "O"
	show Empty = " "