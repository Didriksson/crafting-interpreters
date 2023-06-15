module Scanner(scanTokens) where

data TokenType = LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE |
  COMMA | DOT | MINUS | PLUS| SEMICOLON| SLASH| STAR|
--   One or two character tokens.
  BANG| BANG_EQUAL|
  EQUAL| EQUAL_EQUAL|
  GREATER| GREATER_EQUAL|
  LESS| LESS_EQUAL |

-- Literals.
  IDENTIFIER| STRING| NUMBER|

-- Keywords.

  AND| CLASS| ELSE| FALSE| FUN| FOR| IF| NIL| OR|
  PRINT| RETURN| SUPER| THIS| TRUE| VAR| WHILE |
  EOF
  deriving(Show)

data Token = Token TokenType String String Int | LeftParen | RightParen | LeftBrace | RightBrace | Comma | Dot | Minus | Plus | Semicolon | Slash | Star | Eof
    deriving(Show)

type Error = String

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



scan :: String -> [Token] -> [Token]
scan input tokens =
    case result of
        ([], token) -> tokens ++ [token]
        (remaining, token) -> scan remaining $ tokens ++ [token]
    where result = scanToken input

scanTokens input =
    scan input []