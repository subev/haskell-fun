module NormalForm where

data FictionT = Fiction deriving Show
data NonfictionT = Nonfiction deriving Show

data BookType = FictionBook FictionT
              | NonfictionBook NonfictionT
              deriving Show

type AuthorName = String

data Author = Author (AuthorName, BookType)
  deriving Show

data AuthorTwo =
    FictionT AuthorName
  | NonfictionT AuthorName
  deriving Show

{-another example-}
data Expr = Number Int
          | Add Expr Expr
          | Minus Expr
          | Mult Expr Expr
          | Devide Expr Expr



{-ex 1-}
{-data FlowerType = Gardenia-}
                {-| Daisy-}
                {-| Rose-}
                {-| Lilac-}
                {-deriving Show-}

type Gardener = String

{-data Garden =-}
  {-Garden Gardener FlowerType-}
  {-deriving Show-}

data Garden2 = Gardenia Gardener
             |  Daisy Gardener
             |  Rose Gardener
             |  Lilac Gardener
             deriving Show
