module Expr where

import Source
import qualified Lex as L
import Lex(Token, Token_, OpType(..))

data Stmt a = 
    Def a String (Expr a)
    | Eval a (Expr a)
    | Program a [Stmt a]
    deriving (Show, Eq)

data Expr a = 
    Lit a Int
    | Fn a (Expr a) (Expr a)
    | FnParam a String
    | App a (Expr a) (Expr a)
    | BinOp a OpType (Expr a) (Expr a)
    | Var a String
    | If a (Expr a) (Expr a) (Expr a)
    deriving (Show, Eq)
    
instance Functor Expr where
    fmap f (Lit a t) = Lit (f a) t
    fmap f (Fn a param e) = Fn (f a) (fmap f param) (fmap f e)
    fmap f (FnParam a t) = FnParam (f a) t
    fmap f (App a e1 e2) = App (f a) (fmap f e1) (fmap f e2)
    fmap f (BinOp a t e1 e2) = BinOp (f a) t (fmap f e1) (fmap f e2)
    fmap f (Var a t) = Var (f a) t
    fmap f (If a e1 e2 e3) = If (f a) (fmap f e1) (fmap f e2) (fmap f e3)
   
instance Foldable Expr where
    foldMap f (Lit a _) = f a
    foldMap f (Fn a param e) = f a <> foldMap f param <> foldMap f e
    foldMap f (FnParam a _) = f a
    foldMap f (App a e1 e2) = f a <> foldMap f e1 <> foldMap f e2
    foldMap f (BinOp a _ e1 e2) = f a <> foldMap f e1 <> foldMap f e2
    foldMap f (Var a _) = f a
    foldMap f (If a e1 e2 e3) = f a <> foldMap f e1 <> foldMap f e2 <> foldMap f e3
    
instance Traversable Expr where
    traverse f (Lit a t) = Lit <$> f a <*> pure t
    traverse f (Fn a param e) = Fn <$> f a <*> traverse f param <*> traverse f e
    traverse f (FnParam a t) = FnParam <$> f a <*> pure t
    traverse f (App a e1 e2) = App <$> f a <*> traverse f e1 <*> traverse f e2
    traverse f (BinOp a t e1 e2) = BinOp <$> f a <*> pure t <*> traverse f e1 <*> traverse f e2
    traverse f (Var a t) = Var <$> f a <*> pure t
    traverse f (If a e1 e2 e3) = If <$> f a <*> traverse f e1 <*> traverse f e2 <*> traverse f e3
    
instance Functor Stmt where
    fmap f (Def a t e) = Def (f a) t (fmap f e)
    fmap f (Eval a e) = Eval (f a) (fmap f e)
     
instance Foldable Stmt where
    foldMap f (Def a _ e) = f a <> foldMap f e
    foldMap f (Eval a e) = f a <> foldMap f e
    foldMap f (Program a es) = f a <> foldMap (foldMap f) es
    
instance Traversable Stmt where
    traverse f (Def a t e) = Def <$> f a <*> pure t <*> traverse f e
    traverse f (Eval a e) = Eval <$> f a <*> traverse f e 
    traverse f (Program a es) = Program <$> f a <*> traverse (traverse f) es


getExprValue :: Expr a -> a
getExprValue (Lit a _) = a
getExprValue (Fn a _ _) = a
getExprValue (App a _ _) = a
getExprValue (BinOp a _ _ _) = a
getExprValue (Var a _) = a
getExprValue (If a _ _ _) = a

getStmtValue :: Stmt a -> a
getStmtValue (Def a _ _) = a
getStmtValue (Eval a _) = a
getStmtValue (Program a _) = a

exprLoc :: Expr Span -> Span
exprLoc e = getExprValue e

stmtLoc :: Stmt Span -> Span
stmtLoc s = getStmtValue s

litValue :: Token -> Int
litValue t = case value t of
    L.Lit i -> i
    _ -> error "Expected Lit token"
    
identValue :: Token -> String
identValue t = case value t of
    L.Ident s -> s
    _ -> error "Expected Ident token"
    
mkLit :: Token -> Expr Span
mkLit t = let location = loc t in Lit location (litValue t)

mkFn :: [Token] -> Expr Span -> Expr Span
mkFn [t] e = let location = joinLocations (loc t) (exprLoc e) in Fn location (mkFnParam t) e
mkFn (t:ts) e = let location = joinLocations (loc t) (exprLoc e) in Fn location (mkFnParam t) (mkFn ts e)

mkFnParam :: Token -> Expr Span
mkFnParam t = let location = loc t in FnParam location (identValue t)

mkApp :: Expr Span -> Expr Span -> Expr Span
mkApp lhs rhs = let location = joinLocations (exprLoc lhs) (exprLoc rhs) in App location lhs rhs

getOpType :: Token -> OpType
getOpType t = case value t of
    L.Op op -> op
    _ -> error "Expected Op token"


mkBinOp :: Token -> Expr Span -> Expr Span -> Expr Span
mkBinOp t e1 e2 = let location = joinLocations (exprLoc e1) (exprLoc e2) in BinOp location (getOpType t) e1 e2

mkVar :: Token -> Expr Span
mkVar t = let location = loc t in Var location (identValue t)

mkIf :: Expr Span -> Expr Span -> Expr Span -> Expr Span
mkIf e1 e2 e3 = let location = joinLocations (exprLoc e1) (exprLoc e3) in If location e1 e2 e3

mkDef :: Token -> Expr Span -> Stmt Span
mkDef t e = let location = joinLocations (loc t) (exprLoc e) in Def location (identValue t) e

mkEval :: Expr Span -> Stmt Span
mkEval e = let location = exprLoc e in Eval location e

mkProgram :: [Stmt Span] -> Stmt Span
mkProgram stmts = let location = joinLocations (stmtLoc (head stmts)) (stmtLoc (last stmts)) in Program location stmts