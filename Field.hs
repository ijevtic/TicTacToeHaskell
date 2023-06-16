module Field where

data Field = X | O | EmptyF deriving (Eq)

instance Show Field where
	show X = "X"
	show O = "O"
	show EmptyF = " "