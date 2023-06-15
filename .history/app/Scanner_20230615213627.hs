module Scanner(scanTokens) where

data Result = LeftParen | RightParen | LeftBrace | RightBrace | Comma | Dot | Minus | Plus | Semicolon | Slash | Star | EoF | Error String Int 
    deriving(Show)

scanToken :: String -> (String, Result)
scanToken token = 
  case token of
    ('(':xs) -> (xs, LeftParen)
    (')':xs) -> (xs, RightParen)
    ('{':xs) -> (xs, LeftBrace)
    ('}':xs) -> (xs, RightBrace)
    (',':xs) -> (xs, Comma)
    ('.':xs) -> (xs, Dot)
    ('-':xs) -> (xs, Minus)
    ('+':xs) -> (xs, Plus)
    (';':xs) -> (xs, Semicolon)
    ('*':xs) -> (xs, Star)
    (unknown:xs) -> (xs, Error ("Unexpected character '" ++ [unknown] ++ "'") 1)
    _ -> ("", EoF)

scan :: String -> [Result] -> [Result]
scan input tokens =
    case result of
        ([], token) -> tokens ++ [token]
        (remaining, token) -> scan remaining $ tokens ++ [token]
    where result = scanToken input

scanTokens :: String -> [Result]
scanTokens input =
    scan input []