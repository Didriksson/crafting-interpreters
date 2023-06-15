module Scanner(scanTokens) where

data Token = LeftParen | RightParen | LeftBrace | RightBrace | Comma | Dot | Minus | Plus | Semicolon | Slash | Star | Eof
    deriving(Show)

data Error = MkError String Int deriving(Show)
data Result = Token | Error

scanToken :: String -> (String, Token)
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
    _ -> ("", Eof)

scan :: String -> [Token] -> [Result]
scan input tokens =
    case result of
        ([], token) -> tokens ++ [token]
        (remaining, token) -> scan remaining $ tokens ++ [token]
    where result = scanToken input

scanTokens :: String -> [Result]
scanTokens input =
    scan input []