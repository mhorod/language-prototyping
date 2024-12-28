module Index where

import Expr
import Control.Monad.State 

data Indexed i a = Indexed i a deriving (Show, Eq)

addIndex :: a -> State Int (Indexed Int a)
addIndex a = do
    i <- get
    put (i + 1)
    return (Indexed i a)
    
indexProgram :: Stmt a -> Stmt (Indexed Int a)
indexProgram program = evalState (mapM addIndex program) 0

