module Main where

import Lex
import Parse
import Source
import Expr
import Index
import NameResolution
import Print
import Compile

import System.Environment

example :: String
example = "def f = fn x => x"


main :: IO ()
main = do
    args <- getArgs
    input <- readFile (head args)
    let tokens = Lex.lex input
    mapM_ print tokens
    let program = Parse.parseProgram tokens
    case program of
        Parse.Ok p -> do
            putStr $ printProgram p
            let indexed = indexProgram p
            putStr $ printProgram indexed
            let resolved = resolveNames indexed
            print resolved
            case resolved of
                NameResolution.Ok nr -> do
                    print nr
                    let compiled = compileProgram nr indexed
                    putStr compiled
                    writeFile "out.asm" compiled
                NameResolution.Error e -> print e
            return ()
        Parse.Error e -> print e
        _ -> print "sus"
    return ()