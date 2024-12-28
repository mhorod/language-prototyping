module NameResolution where
    
import qualified Data.Map as Map
import Control.Monad.State

import qualified Expr as E
import Index
import Source
import Lex(Token_(Ident))


type Stmt = E.Stmt (Indexed Int Span)
type Expr = E.Expr (Indexed Int Span)
type NR = Map.Map Int Int

data NameResolutionResult = Ok NR | Error String deriving (Show, Eq)

data NameResolutionContext = NameResolutionContext {
    scopeStack :: [Map.Map String Int]
} deriving (Show, Eq)

openScope :: State NameResolutionContext ()
openScope = do
    NameResolutionContext stack <- get
    put $ NameResolutionContext (Map.empty : stack)

closeScope :: State NameResolutionContext ()
closeScope = do
    NameResolutionContext stack <- get
    case stack of
        [] -> Prelude.error "Cannot close empty stack"
        (scope : rest) -> put $ NameResolutionContext rest

lookupVar :: String -> State NameResolutionContext (Maybe Int)
lookupVar name = do
    NameResolutionContext stack <- get
    return $ lookupVar' stack
    where
        lookupVar' [] = Nothing
        lookupVar' (scope : rest) = case Map.lookup name scope of
            Just i -> Just i
            Nothing -> lookupVar' rest

addVar :: String -> Int -> State NameResolutionContext ()
addVar name i = do
    NameResolutionContext stack <- get
    case stack of
        [] -> Prelude.error "Cannot add variable to empty stack"
        (scope : rest) -> put $ NameResolutionContext ((Map.insert name i scope) : rest)

resolveNames :: Stmt -> NameResolutionResult
resolveNames stmt = evalState (resolveNames' stmt) (NameResolutionContext [])

resolveNames' :: Stmt -> State NameResolutionContext NameResolutionResult
resolveNames' (E.Program _ stmts) = do
    openScope
    mapM_ addDef stmts
    results <- mapM resolveNames' stmts
    closeScope
    return $ combineResults results
resolveNames' (E.Def _ _ expr) = resolveNamesExpr expr
resolveNames' (E.Eval _ expr) = resolveNamesExpr expr

addDef :: Stmt -> State NameResolutionContext ()
addDef (E.Def (Indexed i _) name _) = addVar name i
addDef _ = return ()



combineResults :: [NameResolutionResult] -> NameResolutionResult
combineResults results = foldr combineResults' (Ok Map.empty) results
    where
        combineResults' (Ok m1) (Ok m2) = Ok (Map.union m1 m2)
        combineResults' (Error e) _ = Error e
        combineResults' _ (Error e) = Error e

resolveNamesExpr :: Expr -> State NameResolutionContext NameResolutionResult
resolveNamesExpr (E.Lit _ _) = return $ Ok Map.empty
resolveNamesExpr (E.Fn _ param body) = do
    openScope
    resolveNamesExpr param
    result <- resolveNamesExpr body
    closeScope
    return result
    
resolveNamesExpr (E.FnParam (Indexed i _) name) = do
    addVar name i
    return $ Ok Map.empty
    
resolveNamesExpr (E.App _ e1 e2) = do
    r1 <- resolveNamesExpr e1
    r2 <- resolveNamesExpr e2
    return $ combineResults [r1, r2]
    
resolveNamesExpr (E.BinOp _ _ e1 e2) = do
    r1 <- resolveNamesExpr e1
    r2 <- resolveNamesExpr e2
    return $ combineResults [r1, r2]
    
resolveNamesExpr (E.Var (Indexed i _) name) = do
    m <- lookupVar name
    case m of
        Just j -> return $ Ok (Map.singleton i j)
        Nothing -> return $ Error $ "Variable " ++ name ++ " not in scope"
        
resolveNamesExpr (E.If _ e1 e2 e3) = do
    r1 <- resolveNamesExpr e1
    r2 <- resolveNamesExpr e2
    r3 <- resolveNamesExpr e3
    return $ combineResults [r1, r2, r3]
    