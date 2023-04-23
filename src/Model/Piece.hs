module Model.Piece
    ( Piece(..)
    ) where

data Piece
    = BR
    | BN
    | BB
    | BQ
    | BK
    | BP
    | WR
    | WN
    | WB
    | WQ
    | WK
    | WP
    deriving (Eq, Show)
