module Main where
import System.Environment (getArgs)
import System.IO
import qualified Scanner
import qualified Parser
import qualified Interpreter

run :: String -> IO ()
run input = do
    let tokens = Scanner.scanTokens input
    let expressions = Parser.parse tokens
    print expressions
    putStrLn $ Interpreter.interpret expressions
    

runPrompt :: IO ()
runPrompt = do
    putStr ">"
    hFlush stdout
    input <- getLine
    run input
    runPrompt

runFile :: String -> IO ()
runFile file = do
    content <- readFile file
    run content

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runPrompt
        [file] ->  runFile file
        _ -> putStrLn "Usage: hlox [script]"
