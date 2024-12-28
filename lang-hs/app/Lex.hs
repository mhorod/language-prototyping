module Lex(
    Token,
    Token_(..),
    OpType(..),
    Lex.lex,
) where

import Source
import Control.Monad.State 
import Data.Char

type RawToken = Located RawToken_

data RawToken_ = 
    RawIdent String
    | RawLit Int
    | RawSym String
    | RawWhitespace String
    | RawComment String
    deriving (Show, Eq)

type Token = Located Token_

data Token_ =
    KwDef
    | KwFn
    | KwIf
    | KwThen
    | KwElse
    | CmdEval
    | Ident String
    | Lit Int
    | OpenParen
    | CloseParen
    | SymEq
    | SymFatArrow
    | Op OpType
    | Whitespace String
    | Comment String
    deriving (Show, Eq)
   
data OpType = OpPlus | OpMinus | OpMul | OpEq deriving (Show, Eq)


    
data LexerState = LexerState {
    input :: String,
    pos :: Int
} deriving (Show, Eq)

type Lexer a = State LexerState a    

lex :: String -> [Token]
lex input = runLexer lexer input

runLexer :: Lexer a -> String -> a
runLexer lexer input = evalState lexer (LexerState input 0)

lexer :: Lexer [Token]
lexer = 
    (cook . filterTokens)
    <$>
    programLexer
    
filterTokens :: [RawToken] -> [RawToken]
filterTokens = filter (\t -> case value t of
    RawWhitespace _ -> False
    RawComment _ -> False
    _ -> True)
    
cook :: [RawToken] -> [Token]
cook = map $ fmap (\t -> case t of
    RawIdent s -> cookIdent s
    RawLit i -> Lit i
    RawSym s -> cookSym s
    _ -> error "Unexpected token")

cookIdent :: String -> Token_
cookIdent s = case s of
    "def" -> KwDef
    "fn" -> KwFn
    "if" -> KwIf
    "then" -> KwThen
    "else" -> KwElse
    "eval" -> CmdEval
    _ -> Ident s
    
cookSym :: String -> Token_
cookSym s = case s of
    "(" -> OpenParen
    ")" -> CloseParen
    "=" -> SymEq
    "=>" -> SymFatArrow
    "+" -> Op OpPlus
    "-" -> Op OpMinus
    "*" -> Op OpMul
    "==" -> Op OpEq
    _ -> error "Unexpected symbol"

programLexer :: Lexer [RawToken]
programLexer = do
    LexerState input pos <- get
    case input of
        [] -> return []
        _ -> do
            token <- rawTokenLexer
            tokens <- programLexer
            return (token : tokens)

rawTokenLexer :: Lexer RawToken
rawTokenLexer = do
    LexerState input pos <- get
    case input of 
        [] -> error "Unexpected end of input"
        (c:cs)
            | isAlpha c -> identLexer
            | isSpace c -> whitespaceLexer
            | isDigit c -> litLexer
            | c == '#' -> commentLexer
            | c `elem` ['(', ')', '=', '-', '+', '*'] -> symbolLexer

  
  
litLexer :: Lexer RawToken
litLexer = fmap (fmap (RawLit . read)) (Lex.takeWhile isDigit)
          
identLexer :: Lexer RawToken
identLexer = fmap (fmap RawIdent) (Lex.takeWhile isIdentContinue)

isIdentContinue :: Char -> Bool
isIdentContinue c = isAlpha c || isDigit c || c == '_'


whitespaceLexer :: Lexer RawToken
whitespaceLexer = fmap (fmap RawWhitespace) (Lex.takeWhile (\c -> c `elem` [' ', '\t', '\n']))

symbolLexer :: Lexer RawToken
symbolLexer = do
    LexerState input pos <- get
    case input of
        '(' : rest -> returnSymbol "("
        ')' : rest -> returnSymbol ")"
        '=' : '>' : rest -> returnSymbol "=>"
        '=' : '=' : rest -> returnSymbol "=="
        '=' : rest -> returnSymbol "="
        '-' : rest -> returnSymbol "-"
        '+' : rest -> returnSymbol "+"
        '*' : rest -> returnSymbol "*"
        _ -> error "Unexpected symbol"


returnSymbol :: String -> Lexer RawToken
returnSymbol s = do
    LexerState input pos <- get
    put $ LexerState (drop (length s) input) (pos + length s)
    return $ Located (Span pos (pos + length s)) (RawSym s)

takeWhile :: (Char -> Bool) -> Lexer (Located String)
takeWhile p = do
    LexerState input pos <- get
    let (match, rest) = span p input
    let len = length match
    put $ LexerState rest (pos + len)
    return $ Located (Span pos (pos + len)) match

commentLexer :: Lexer RawToken
commentLexer = do
    LexerState input pos <- get
    let (comment, rest) = span (/= '\n') input
    let len = length comment
    put $ LexerState rest (pos + len)
    return $ Located (Span pos (pos + len)) (RawComment comment)