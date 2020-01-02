module ADT where

data Expr = Num Integer | Name String | List [Expr] 
    deriving (Show)