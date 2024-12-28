module Source where

data Span = Span { start :: Int, end :: Int }
    deriving (Show, Eq)
    
data Located a = Located { loc :: Span, value :: a }
    deriving (Show, Eq)
    
    
instance Functor Located where
    fmap f (Located loc a) = Located loc (f a)
    
    
joinLocations :: Span -> Span -> Span
joinLocations (Span s1 e1) (Span s2 e2) = Span (min s1 s2) (max e1 e2)