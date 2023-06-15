module Scanner(scanTokens) where

data Result = 
    LeftParen | RightParen | LeftBrace | RightBrace | Comma | Dot | Minus | Plus | Semicolon | Slash | Star | EoF |
    Comment String |
    BangEqual |
    EqualEqual |
    LessEqual |
    GreaterEqual |
    Error String Int 
    deriving(Show)

dropComment :: String -> String
dropComment = dropWhile (/= '\n')
    

scanToken :: String -> Int -> (String, Result)
scanToken token lines = 
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
    ('/':'/':xs) -> (dropComment xs, Comment (dropComment xs)) 
    ('\n':xs) -> scanToken xs lines
    ('\t':xs) -> scanToken xs lines
    ('\r':xs) -> scanToken xs lines
    (unknown:xs) -> (xs, Error ("Unexpected character '" ++ [unknown] ++ "'") lines)
    _ -> ("", EoF)

scan :: String -> [Result] -> Int -> [Result]
scan input tokens lines =
    case result of
        ([], token) -> tokens ++ [token]
        (remaining, Comment comment) -> scan remaining (tokens ++ [token]) (lines + 1 )
        (remaining, token) -> scan remaining (tokens ++ [token]) lines
    where result = scanToken input lines

scanTokens :: String -> [Result]
scanTokens input =
    scan input [] 1