module Print where

import Control.Monad.State

import Expr
import Source

indent :: State Int ()
indent = modify (+1)

dedent :: State Int ()
dedent = modify (subtract 1)

showIndented :: String -> State Int String
showIndented s = do
    i <- get
    return $ replicate i ' ' ++ s


printStmt :: Show a => Stmt a -> State Int String
printStmt (Def a t e) = do
    indent
    e' <- printExpr e
    dedent
    showIndented $ "Def " ++ show a ++ " " ++ show t ++ " \n" ++ e'
    
printStmt (Eval a e) = do
    indent
    e' <- printExpr e
    dedent
    showIndented $ "Eval " ++ show a ++ " \n" ++ e'
    
printStmt (Program a stmts) = do
    indent
    stmts' <- mapM printStmt stmts
    dedent
    showIndented $ "Program " ++ show a ++ " \n" ++ concat stmts'
    
printExpr :: Show a => Expr a -> State Int String
printExpr (Lit a t) = showIndented $ "Lit " ++ show a ++ " " ++ show t ++ " \n"
printExpr (Fn a param e) = do
    indent
    param' <- printExpr param
    e' <- printExpr e
    dedent
    showIndented $ "Fn " ++ show a ++ " \n" ++ param' ++ e'
    
printExpr (FnParam a t) = showIndented $ "FnParam " ++ show a ++ " " ++ show t ++ " \n"
printExpr (App a e1 e2) = do
    indent
    e1' <- printExpr e1
    e2' <- printExpr e2
    dedent
    showIndented $ "App " ++ show a ++ " \n" ++ e1' ++ e2'
    
printExpr (BinOp a t e1 e2) = do
    indent
    e1' <- printExpr e1
    e2' <- printExpr e2
    dedent
    showIndented $ "BinOp " ++ show a ++ " " ++ show t ++ " \n" ++ e1' ++ e2'
    
printExpr (Var a t) = showIndented $ "Var " ++ show a ++ " " ++ show t ++ " \n"
printExpr (If a e1 e2 e3) = do
    indent
    e1' <- printExpr e1
    e2' <- printExpr e2
    e3' <- printExpr e3
    dedent
    showIndented $ "If " ++ show a ++ " \n" ++ e1' ++ e2' ++ e3'
    
printProgram :: Show a => Stmt a -> String
printProgram stmt = evalState (printStmt stmt) 0