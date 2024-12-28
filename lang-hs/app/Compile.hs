module Compile(compileProgram) where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import NameResolution
import Lex(Token, OpType(..))
import qualified Expr as E
import Index
import Source

{- 
Compile a program to asm string

Functions are currified so that each function takes exactly one argument.
fn x y z => T becomes fn x => fn y => fn z => T

Representation in memory:
    integers are 64 bits
    functions are 64 bit pointers to a closure object in memory
    

Closure object layout:
    0: pointer to 2 argument function 
    8: pointer to parent closure object
    16: closure argument

Function code:
    Arguments:
        rdi contains pointer to closure object
        rsi contains argument provided argument

Assuming that pointer to closure is in rdi and the argument is rsi then call is simly done by
    call [rdi]

For each fn we create a two argument function:
.fn-{id}: (rdi, rsi)
    mov [rdi + 16], rsi ; save the argument so sub-closures can use it
    
    
Global values defined by def are converted to a lazy 0-argument function:
    def x = body
    becomes
    
    .def-x:
        {body}
        ret
    
    To access the value we simply call the function and the result is in rax 

Generating variable access:
    - If the variable is bound to the current closure then we simply perform
        mov rax, [rdi + 16]
    - If the variable x is a global `def` name then we can get it by label 
        call .def-x
    - If the variable x isn't bound in the current closure then we need to access it via parent closure
        mov rax, rdi
        mov rax, [rax + 8] ; get parent closure
        mov rax, [rax + 8] ; get parent parent closure
        ...
        mov rax, [rax + 16] ; get the value from ancestor closure
    
-}

data CompilationContext = CompilationContext {
    nr :: NR,
    defs :: Set.Set Int
}

data CompilationState = CompilationState {
    nextLabel :: Int,
    fnParamStack :: [Int]
}

newLabel :: State CompilationState Int
newLabel = do
    s <- get
    put $ s { nextLabel = nextLabel s + 1 }
    return $ nextLabel s


fnLabel :: Expr -> String
fnLabel (E.Fn (Indexed i _) _ _) = ".fn" ++ show i
fnLabel _ = error "Expected Fn"

defLabel :: Stmt -> String
defLabel (E.Def (Indexed i _) _ _) = ".def" ++ show i
defLabel _ = error "Expected Def"

compileProgram :: NR -> Stmt -> String
compileProgram nr stmt = let ctx = CompilationContext nr (getDefs stmt) in
    evalState (compileStmt ctx stmt) (CompilationState 0 [])


getDefs :: Stmt -> Set.Set Int
getDefs (E.Program _ stmts) = Set.unions $ map getDefs stmts
getDefs (E.Def (Indexed i _) _ _) = Set.singleton i
getDefs _ = Set.empty

compileStmt :: CompilationContext -> Stmt -> State CompilationState String
compileStmt ctx (E.Program _ stmts) = do
    defs <- mapM (compileDef ctx) stmts
    fns <- mapM (compileStmtFns ctx) stmts
    stmts' <- mapM (compileStmt ctx) stmts
    return $
        "section .text\n" ++
        "global _main\n" ++
        "extern print_int\n" ++
        "extern new_closure\n" ++
        "extern clone_closure\n" ++
        concat defs ++
        concat fns ++
        "_main:\n" ++
        concat stmts' ++
        "mov rax, 0\n" ++
        "ret\n"

compileStmt ctx (E.Def (Indexed i _) t e) = return ""
compileStmt ctx (E.Eval (Indexed i _) e) = do
    e' <- compile ctx e
    return $
        "push rdi\n" ++
        e' ++
        "mov rdi, rax\n" ++
        "call print_int\n" ++
        "pop rdi\n"


compileDef :: CompilationContext -> Stmt -> State CompilationState String
compileDef ctx def@(E.Def (Indexed i _) t e) = do
    e' <- compile ctx e
    return $
        defLabel def ++ ":\n" ++
        e' ++
        "ret\n"
compileDef _ _ = return ""

compileStmtFns :: CompilationContext -> Stmt -> State CompilationState String
compileStmtFns ctx (E.Program _ stmts) = do
    fns <- mapM (compileStmtFns ctx) stmts
    return $ concat fns
compileStmtFns ctx (E.Def (Indexed i _) t e) = compileFns ctx e
compileStmtFns ctx (E.Eval (Indexed i _) e) = compileFns ctx e

compileFns :: CompilationContext -> Expr -> State CompilationState String
compileFns ctx (E.Lit _ _) = return ""
compileFns ctx fn@(E.Fn i param e) = do
    modify $ \s -> s { fnParamStack = (getParamIndex param) : fnParamStack s }
    fn' <- compileFn ctx fn
    e' <- compileFns ctx e
    modify $ \s -> s { fnParamStack = tail $ fnParamStack s }
    return $ fn' ++ e'

compileFns ctx (E.FnParam _ _) = return ""
compileFns ctx (E.App _ e1 e2) = do
    e1' <- compileFns ctx e1
    e2' <- compileFns ctx e2
    return $ e1' ++ e2'
compileFns ctx (E.BinOp _ _ e1 e2) = do
    e1' <- compileFns ctx e1
    e2' <- compileFns ctx e2
    return $ e1' ++ e2'
compileFns ctx (E.Var _ _) = return ""
compileFns ctx (E.If _ e1 e2 e3) = do
    e1' <- compileFns ctx e1
    e2' <- compileFns ctx e2
    e3' <- compileFns ctx e3
    return $ e1' ++ e2' ++ e3'


getParamIndex :: Expr -> Int
getParamIndex (E.FnParam (Indexed i _) _) = i
getParamIndex _ = error "Expected FnParam"

compileFn :: CompilationContext -> Expr -> State CompilationState String
compileFn ctx (E.Fn (Indexed i _) param e) = do
    e' <- compile ctx e
    return $
        ".fn" ++ show i ++ ":\n" ++
        "push rsi\n" ++
        "call clone_closure\n" ++
        "pop rsi\n" ++
        "mov rdi, rax\n" ++
        e' ++
        "ret\n"
compileFn _ _ = error "Expected Fn"

compile :: CompilationContext -> Expr -> State CompilationState String
compile ctx (E.Lit _ i) = return $ "mov rax, " ++ show i ++ "\n"
compile ctx (E.Fn i param e) = return $
    -- Evaluating fn should create a new closure object
    "push rdi\n" ++
    "push rsi\n" ++
    -- In rdi we have pointer to current closure object (which is parent of the new closure)
    "lea rsi, [rel " ++ fnLabel (E.Fn i param e) ++ "]\n" ++
    "call new_closure\n" ++
    "pop rsi\n" ++
    "pop rdi\n"

compile ctx (E.FnParam i t) = error "Cannot compile FnParam"
compile ctx (E.App _ e1 e2) = do
    e1' <- compile ctx e1
    e2' <- compile ctx e2
    return $
        "push rdi\n" ++
        "push rsi\n" ++
        e1' ++
        "push rax\n" ++
        e2' ++
        "pop rdi\n" ++
        "mov rsi, rax\n" ++
        "call [rdi]\n" ++
        "pop rsi\n" ++
        "pop rdi\n"


compile ctx (E.BinOp _ t e1 e2) = do
    e1' <- compile ctx e1
    e2' <- compile ctx e2
    return $
        e1' ++
        "push rax\n" ++
        e2' ++
        "pop rdx\n" ++
        case t of
            OpPlus -> "add rax, rdx\n"
            OpMinus -> "sub rdx, rax\n" ++ "mov rax, rdx\n"
            OpMul -> "imul rax, rdx\n"
            OpEq -> "cmp rax, rdx\n" ++
                "sete al\n" ++
                "movzx rax, al\n"

compile ctx (E.Var (Indexed i _) _) = do
    s <- get
    let resolved = nr ctx Map.! i
    if Set.member resolved (defs ctx) then
        return $ 
        "push rdi\n" ++
        "push rsi\n" ++
        "call " ++ (".def" ++ show resolved) ++ "\n" ++
        "pop rsi\n" ++
        "pop rdi\n"
    else
        return $ generateLocalVarAccess s resolved

compile ctx (E.If _ e1 e2 e3) = do
    falseLabel <- newLabel
    endLabel <- newLabel
    e1' <- compile ctx e1
    e2' <- compile ctx e2
    e3' <- compile ctx e3
    return $
        e1' ++
        "test rax, rax\n" ++
        "jz .if" ++ show falseLabel ++ "end\n" ++
        e2' ++
        "jmp .if" ++ show endLabel ++ "end\n" ++
        ".if" ++ show falseLabel ++ "end:\n" ++
        e3' ++
        ".if" ++ show endLabel ++ "end:\n"

generateLocalVarAccess :: CompilationState -> Int -> String
generateLocalVarAccess s i =
    if i == head (fnParamStack s) then
        "mov rax, rsi\n"
    else
        "mov rax, rdi\n" ++ generateLocalVarAccess' (fnParamStack s) i

generateLocalVarAccess' :: [Int] -> Int -> String
generateLocalVarAccess' [] i = error $ "Variable " ++ show i ++  "not found"
generateLocalVarAccess' (x:xs) i =
    if x == i then
        "mov rax, [rax + 16]\n"
    else
        "mov rax, [rax + 8]\n" ++ generateLocalVarAccess' xs i