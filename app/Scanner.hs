module Scanner(scanTokens, Token (..), TokenType(..)) where
import Data.Char ( isDigit, isAlpha, isAlphaNum )
data Token = Token TokenType Int deriving (Show)
data TokenType = 
    LeftParen | RightParen | LeftBrace | RightBrace | Comma | Dot | Minus | Plus | Semicolon | Slash | Star | EoF |
    Bang |
    Equal |
    Less |
    Greater |
    Comment String |
    STRING String |
    NUMBER Float |
    BangEqual |
    EqualEqual |
    LessEqual |
    Class |
    Else |
    FALSE |
    TRUE |
    For |
    Fun |
    Or |
    If |
    Nil |
    Print |
    Return |
    Super |
    This |
    Var |
    While |
    Identifier String |
    Error String |
    And |
    GreaterEqual |
    Newline
    deriving(Show, Eq)

dropComment :: String -> String
dropComment = dropWhile (/= '\n')

takeComment :: String -> String
takeComment = takeWhile (/= '\n')
    
parseString :: String -> (String, TokenType)
parseString input =
    (rest, STRING rSTRING)
    where 
        rSTRING = takeWhile (/= '"') input
        rest = drop (length rSTRING + 1) input

parseFloat :: String -> Float
parseFloat raw = read raw :: Float

parseNumber :: String -> (String, TokenType)
parseNumber input =
    case rest of
        '.':p:xs -> if isDigit p then
                        (dropWhile isDigit xs, NUMBER (parseFloat (rNumber ++ "." ++ [p] ++ takeWhile isDigit xs)))
                    else 
                        (p : xs, NUMBER (parseFloat (rNumber ++ "." ++ takeWhile isDigit xs)))
        _ -> (rest, NUMBER (parseFloat rNumber))
    where 
        rNumber = takeWhile isDigit input
        rest = drop (length rNumber) input

parseIdentifierOrKeyword :: String -> (String, TokenType)
parseIdentifierOrKeyword input = 
    case rIdentifier of
        "and" -> (rest, And)
        "class" -> (rest, Class)
        "else" -> (rest, Else)
        "false" -> (rest, FALSE)
        "for" -> (rest, For)
        "fun" -> (rest, Fun)
        "if" -> (rest, If)
        "nil" -> (rest, Nil)
        "or" -> (rest, Or)
        "print" -> (rest, Print)
        "return" -> (rest, Return)
        "super" -> (rest, Super)
        "this" -> (rest, This)
        "true" -> (rest, TRUE)
        "var" -> (rest, Var)
        "while" -> (rest, While)
        _ -> (rest, Identifier rIdentifier)
    where 
        rIdentifier = takeWhile isAlphaNum input
        rest = drop (length rIdentifier) input

scanToken :: String -> (String, TokenType)
scanToken token = 
  case token of
    '(':xs -> (xs, LeftParen)
    ')':xs -> (xs, RightParen)
    '{':xs -> (xs, LeftBrace)
    '}':xs -> (xs, RightBrace)
    ',':xs -> (xs, Comma)
    '.':xs -> (xs, Dot)
    '-':xs -> (xs, Minus)
    '+':xs -> (xs, Plus)
    ';':xs -> (xs, Semicolon)
    '*':xs -> (xs, Star)
    '!':'=':xs -> (xs, BangEqual)
    '!':xs -> (xs, Bang)
    '=':'=':xs -> (xs, EqualEqual)
    '=':xs -> (xs, Equal)
    '<':'=':xs -> (xs, LessEqual)
    '<':xs -> (xs, Less)
    '>':'=':xs -> (xs, GreaterEqual)
    '>':xs -> (xs, Greater)
    '"':xs -> parseString xs
    '/':'/':xs -> (dropComment xs, Comment (takeComment xs))
    '/':xs -> (xs, Slash)
    ' ':xs -> scanToken xs
    'O':'R':xs -> (xs, Or)
    'A':'N':'D':xs -> (xs, And)
    '\n':xs -> (xs, Newline)
    '\t':xs -> scanToken xs
    '\r':xs -> scanToken xs    
    unknown:xs -> if isDigit unknown 
                    then parseNumber token
                  else if isAlpha unknown
                    then parseIdentifierOrKeyword token
                    else (xs, Error ("Unexpected character '" ++ [unknown] ++ "'"))
    _ -> ("", EoF)

scan :: String -> [Token] -> Int -> [Token]
scan input tokens currentLine =
    case result of
        ([], EoF) -> tokens ++ [Token EoF currentLine]
        (remaining, Comment _) -> scan remaining tokens currentLine
        (remaining, Newline) -> scan remaining tokens (currentLine + 1 )
        (remaining, tokentype) -> scan remaining (tokens ++ [Token tokentype currentLine]) currentLine
    where result = scanToken input

scanTokens :: String -> [Token]
scanTokens input =
    scan input [] 1