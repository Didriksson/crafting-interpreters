module Scanner(scanTokens) where

data Result = 
    LeftParen | RightParen | LeftBrace | RightBrace | Comma | Dot | Minus | Plus | Semicolon | Slash | Star | EoF |
    BangEqual |
    EqualEqual |
    LessEqual |
    GreaterEqual |
    Error String Int 
    deriving(Show)

dropComment :: String -> String
dropComment = dropWhile (/= '\n')
    

scanToken :: String -> Int -> (String, Result, Int)
scanToken token line = 
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
    ('!':'=':xs) -> (xs, BangEqual)
    ('=':'=':xs) -> (xs, EqualEqual)
    ('<':'=':xs) -> (xs, LessEqual)
    ('>':'=':xs) -> (xs, GreaterEqual)
    ('/':'/':xs) -> scanToken (dropComment xs) line
    ('\n':xs) -> scanToken xs $ line + 1
    ('\t':xs) -> scanToken xs line
    ('\r':xs) -> scanToken xs line
    (unknown:xs) -> (xs, Error ("Unexpected character '" ++ [unknown] ++ "'") line)
    _ -> ("", EoF)

scan :: String -> [Result] -> [Result]
scan input tokens =
    case result of
        ([], token) -> tokens ++ [token]
        (remaining, token) -> scan remaining $ tokens ++ [token]
    where result = scanToken input 1

scanTokens :: String -> [Result]
scanTokens input =
    scan input []