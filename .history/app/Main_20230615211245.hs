module Main where
import System.Environment (getArgs)
import System.IO

run :: String -> IO ()
run input = do
    let tokens = Scanner.scanTokens input
    print tokens

runPrompt :: IO ()
runPrompt = do
    putStr ">"
    hFlush stdout
    input <- getLine
    run input
    runPrompt

runFile :: String -> IO ()
runFile file = putStrLn $ "Running file:" <> file

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runPrompt
        [file] ->  runFile file
        _ -> putStrLn "Usage: hlox [script]"
