module Parse(parseProgram, ParsingResult(..)) where

import Control.Monad.State 

import Source    
import Lex
import qualified Expr as E
import Expr (mkLit, mkFn, mkApp, mkBinOp, mkVar, mkIf, mkDef, mkEval, mkProgram) 
    
type Expr = E.Expr Span
type Stmt = E.Stmt Span

data ParsingState = ParsingState {
    tokens :: [Token],
    backtrack :: Bool
} deriving (Show, Eq)

data ParsingResult e a = Ok a | Backtracked | Error e
    deriving (Show, Eq)
    
instance Functor (ParsingResult e) where
    fmap f (Ok a) = Ok (f a)
    fmap f Backtracked = Backtracked
    fmap f (Error e) = Error e
    
instance Applicative (ParsingResult e) where
    pure = Ok
    Ok f <*> Ok a = Ok (f a)
    _ <*> Backtracked = Backtracked
    Backtracked <*> _ = Backtracked
    Error e <*> _ = Error e
    _ <*> Error e = Error e
    
instance Monad (ParsingResult e) where
    return = pure
    Ok a >>= f = f a
    Backtracked >>= _ = Backtracked
    Error e >>= _ = Error e

data Parser e a = Parser {
    parse :: ParsingState -> ParsingResult e (a, ParsingState)
}

instance Functor (Parser e) where
    fmap f (Parser p) = Parser $ \s -> case p s of
        Ok (a, s') -> Ok (f a, s')
        Backtracked -> Backtracked
        Error e -> Error e
        
instance Applicative (Parser e) where
    pure a = Parser $ \s -> Ok (a, s)
    Parser pf <*> Parser pa = Parser $ \s -> case pf s of
        Ok (f, s') -> case pa s' of
            Ok (a, s'') -> Ok (f a, s'')
            Backtracked -> Backtracked
            Error e -> Error e
        Backtracked -> Backtracked
        Error e -> Error e
        
instance Monad (Parser e) where
    return = pure
    Parser pa >>= f = Parser $ \s -> case pa s of
        Ok (a, s') -> let Parser pb = f a in pb s'
        Backtracked -> Backtracked
        Error e -> Error e

parsed :: ParsingResult e a -> Parser e a
parsed (Ok a) = return a
parsed Backtracked = Parser $ \s -> Backtracked
parsed (Error e) = Parser $ \s -> Error e
        
commit :: Parser e ()
commit = Parser $ \s -> Ok ((), s { backtrack = False })

allowBacktrack :: Parser e ()
allowBacktrack = Parser $ \s -> Ok ((), s { backtrack = True })

getBacktrack :: Parser e Bool
getBacktrack = Parser $ \s -> Ok (backtrack s, s)

setBacktrack :: Bool -> Parser e ()
setBacktrack b = Parser $ \s -> Ok ((), s { backtrack = b })

fail :: e -> Parser e a
fail e = do
    Parser $ \s -> if backtrack s then Backtracked else Error e
    
error :: e -> Parser e a
error e = Parser $ \s -> Error e

peek :: Parser e (Maybe Token)
peek = Parser $ \s -> case tokens s of
    [] -> Ok (Nothing, s)
    t : ts -> Ok (Just t, s)
    
peek1 :: Parser String Token
peek1 = do
    m <- peek
    case m of
        Just t -> return t
        _ -> Parse.fail "Unexpected end of input"

consume :: Parser String Token
consume = Parser $ \s -> case tokens s of
    [] -> Error "Unexpected end of input"
    t : ts -> Ok (t, s { tokens = ts })
   
many :: Parser e a -> Parser e [a]
many p = do
    b <- getBacktrack
    allowBacktrack
    result <- many' p
    setBacktrack b
    return result
 
many' :: Parser e a -> Parser e [a]
many' p = Parser $ \s -> case parse p s of
    Ok (a, s') -> case parse (many p) s' of
        Ok (as, s'') -> Ok (a : as, s'')
        Backtracked -> Ok ([a], s')
        Error e -> Error e
    Backtracked -> Ok ([], s)
    Error e -> Error e



exact :: Token_ -> Parser String Token
exact t = do
    m <- peek
    case m of
        Just t' | t == value t' -> consume
        Just t' -> Parse.fail $ "Unexpected token: " ++ show t' ++ " (expected " ++ show t ++ ")"
        _ -> Parse.fail $ "Unexpected end of input"
        
        
match :: (Token_ -> Bool) -> Parser String Token
match p = do
    m <- peek
    case m of
        Just t | p (value t) -> consume
        Just t' -> Parse.fail $ "Unexpected token: " ++ show t' ++ " (expected something that matches)"
        _ -> Parse.fail $ "Unexpected end of input"
        
        
isLit :: Token_ -> Bool
isLit (Lex.Lit _) = True
isLit _ = False

isIdent :: Token_ -> Bool
isIdent (Ident _) = True
isIdent _ = False

parseLit :: Parser String Expr
parseLit = do
    t <- match isLit
    return (mkLit t)
    
    
runParser :: Parser e a -> ParsingState -> ParsingResult e a
runParser p s = fmap fst (parse p s)
    
    
parseProgram :: [Token] -> ParsingResult String Stmt
parseProgram tokens = runParser parseProgram' (ParsingState tokens True)

parseProgram' :: Parser String Stmt
parseProgram' = mkProgram <$> many parseStmt

parseStmt :: Parser String Stmt
parseStmt = do
    t <- peek1
    case value t of
        KwDef -> parseDef
        CmdEval -> parseEval
        _ -> Parse.error $ "Unexpected token: " ++ show t
        
        
data TermPart = TermExpr Expr | TermOp Token | OpApp deriving (Show, Eq)        


parseExpr :: Parser String Expr
parseExpr = do
    parts <- many parseTermPart
    let inserted = insertApp parts
    parsed $ validate inserted
    return $ mkExpr inserted
        
mkExpr :: [TermPart] -> Expr
mkExpr parts = mkExpr' $ ExprState parts [] []

-- Check if the expression is valid, i.e. we can build a legal tree from it
validate :: [TermPart] -> ParsingResult String ()
validate [] = Error "Empty expression"
validate [TermExpr _] = Ok ()
validate (TermOp t : _) = Error $ "Unexpected operator at the beginning of the expression: " ++ show t
validate (TermExpr _ : TermOp _ : rest) = validate rest
validate (TermExpr _ : OpApp : rest) = validate rest
validate ts = Error $ "Unexpected expression: " ++ show ts


data ExprState = ExprState  {
    -- Parts of the expression left to processing
    parts :: [TermPart],
    -- Already processed nodes
    nodes :: [Expr],
    -- The operators, in reverse order, so that we can pop them off the stack in the correct order
    -- Invariant: The precedence of the operators is decreasing
    --            Adding an operator with lower precedence pops the stack until the precedence is lower
    ops :: [TermPart]
} deriving (Show, Eq)


mkExpr' :: ExprState -> Expr
mkExpr' s = case parts s of
    [] -> buildExpr s
    TermExpr e : rest -> mkExpr' $ s { parts = rest, nodes = e : nodes s }
    op : rest -> let s' = s { parts = rest } in mkExpr' (pushOp s' op)

buildExpr :: ExprState -> Expr
buildExpr s = case ops s of
    [] -> head (nodes s)
    _ -> buildExpr (popOp s)
    
 
pushOp :: ExprState -> TermPart -> ExprState
pushOp s op = case ops s of
    [] -> s { ops = [op] }
    t : rest -> if precedence op > precedence t then
        -- If the precedence of the operator is higher than the precedence of the top of the stack, push it
        s { ops = op : ops s }
    else
        -- If the precedence of the operator is lower, pop the stack and push the operator
        pushOp (popOp s) op

-- Pop the top operator off the stack, adding a new node
popOp :: ExprState -> ExprState
popOp s = case ops s of
    [] -> Prelude.error "Cannot pop an empty stack"
    t : restOps -> case nodes s of
        [] -> Prelude.error $  "Cannot pop an operator without nodes: " ++ show s
        [e] -> Prelude.error $ "Cannot pop an operator with only one node: " ++ show s
        e1 : e2 : restNodes -> s { nodes = (mkOp t e2 e1) : restNodes, ops = restOps }
        
mkOp :: TermPart -> Expr -> Expr -> Expr
mkOp OpApp e1 e2 = mkApp e1 e2
mkOp (TermOp t) e1 e2 = mkBinOp t e1 e2
mkOp _ _ _ = Prelude.error "Cannot create an operator from a non-operator"


precedence :: TermPart -> Int
precedence (TermOp t) = case value t of
    Op OpEq -> 1
    Op OpPlus -> 2
    Op OpMinus -> 2
    Op OpMul -> 3
    _ -> Prelude.error "Unknown operator"

precedence OpApp = 4
precedence (TermExpr _) = Prelude.error "Cannot calculate precedence of an expression"


insertApp :: [TermPart] -> [TermPart]
insertApp [] = []
insertApp [x] = [x]
insertApp (TermExpr e1 : TermExpr e2 : rest) = (TermExpr e1) : OpApp : (insertApp ((TermExpr e2) : rest))
insertApp (x:xs) = x : insertApp xs


        
parseTermPart :: Parser String TermPart
parseTermPart = do
    m <- peek1
    if isOp (value m) then
        TermOp <$> parseTermOp
    else
        TermExpr <$> parseTerm

parseTermOp :: Parser String Token
parseTermOp = match isOp

isOp :: Token_ -> Bool
isOp (Op _) = True
isOp _ = False

parseTerm :: Parser String Expr
parseTerm = do
    t <- peek1
    case value t of
        KwFn -> parseFn
        KwIf -> parseIf
        Ident _ -> parseVar
        Lex.Lit _ -> parseLit
        OpenParen -> parseParen
        _ -> Parse.fail $ "Unexpected token, expected term: " ++ show t
        
        
parseParen :: Parser String Expr
parseParen = do
    exact OpenParen
    commit
    e <- parseExpr
    exact CloseParen
    return e
    
parseIdent :: Parser String Token
parseIdent = match isIdent

parseDef :: Parser String Stmt
parseDef = do
    exact KwDef
    commit
    t <- parseIdent
    exact SymEq
    e <- parseExpr
    return (mkDef t e)
    
    
parseFn :: Parser String Expr
parseFn = do
    exact KwFn
    commit
    ts <- many parseIdent
    exact SymFatArrow
    e <- parseExpr
    return (mkFn ts e)
    
parseVar :: Parser String Expr
parseVar = do
    t <- parseIdent
    return (mkVar t)
    
parseIf :: Parser String Expr
parseIf = do
    exact KwIf
    commit
    e1 <- parseExpr
    exact KwThen
    e2 <- parseExpr
    exact KwElse
    e3 <- parseExpr
    return (mkIf e1 e2 e3)
    
parseEval :: Parser String Stmt
parseEval = do
    exact CmdEval
    commit
    e <- parseExpr
    return (mkEval e)