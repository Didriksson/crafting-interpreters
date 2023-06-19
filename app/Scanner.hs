module Scanner(scanTokens) where
import Data.Char ( isDigit, isAlpha, isAlphaNum )


data Result = 
    LeftParen Int| RightParen Int| LeftBrace Int| RightBrace Int| Comma Int| Dot Int| Minus Int| Plus Int| Semicolon Int| Slash Int| Star Int| EoF Int|
    Bang Int |
    Equal Int |
    Less Int |
    Greater Int |
    Comment String Int |
    STRING String Int |
    NUMBER Float Int |
    BangEqual Int |
    EqualEqual Int |
    LessEqual Int |
    Class Int |
    Else Int |
    FALSE Int |
    TRUE Int |
    For Int |
    Fun Int |
    Or Int |
    If Int |
    Nil Int |
    Print Int |
    Return Int |
    Super Int |
    This Int |
    Var Int |
    While Int |
    Identifier String Int |
    And Int|
    GreaterEqual Int|
    Newline Int |
    Error String Int 
    deriving(Show)

dropComment :: String -> String
dropComment = dropWhile (/= '\n')

takeComment :: String -> String
takeComment = takeWhile (/= '\n')
    
parseString :: String -> Int -> (String, Result)
parseString input cline =
    (rest, STRING rSTRING cline)
    where 
        rSTRING = takeWhile (/= '"') input
        rest = drop (length rSTRING + 1) input

parseFloat :: String -> Float
parseFloat raw = read raw :: Float

parseNumber :: String -> Int -> (String, Result)
parseNumber input cline =
    case rest of
        '.':p:xs -> if isDigit p then
                        (dropWhile isDigit xs, NUMBER (parseFloat (rNumber ++ "." ++ [p] ++ takeWhile isDigit xs)) cline)
                    else 
                        (p : xs, NUMBER (parseFloat (rNumber ++ "." ++ takeWhile isDigit xs)) cline)
        _ -> (rest, NUMBER (parseFloat rNumber) cline)
    where 
        rNumber = takeWhile isDigit input
        rest = drop (length rNumber) input

parseIdentifierOrKeyword :: String -> Int -> (String, Result)
parseIdentifierOrKeyword input line = 
    case rIdentifier of
        "and" -> (rest, And line)
        "class" -> (rest, Class line)
        "else" -> (rest, Else line)
        "false" -> (rest, FALSE line)
        "for" -> (rest, For line)
        "fun" -> (rest, Fun line)
        "if" -> (rest, If line)
        "nil" -> (rest, Nil line)
        "or" -> (rest, Or line)
        "print" -> (rest, Print line)
        "return" -> (rest, Return line)
        "super" -> (rest, Super line)
        "this" -> (rest, This line)
        "true" -> (rest, TRUE line)
        "var" -> (rest, Var line)
        "while" -> (rest, While line)
        _ -> (rest, Identifier rIdentifier line)
    where 
        rIdentifier = takeWhile isAlphaNum input
        rest = drop (length rIdentifier) input

scanToken :: String -> Int -> (String, Result)
scanToken token currentLine = 
  case token of
    '(':xs -> (xs, LeftParen currentLine)
    ')':xs -> (xs, RightParen currentLine)
    '{':xs -> (xs, LeftBrace currentLine)
    '}':xs -> (xs, RightBrace currentLine)
    ',':xs -> (xs, Comma currentLine)
    '.':xs -> (xs, Dot currentLine)
    '-':xs -> (xs, Minus currentLine)
    '+':xs -> (xs, Plus currentLine)
    ';':xs -> (xs, Semicolon currentLine)
    '*':xs -> (xs, Star currentLine)
    '!':'=':xs -> (xs, BangEqual currentLine)
    '!':xs -> (xs, Bang currentLine)
    '=':'=':xs -> (xs, EqualEqual currentLine)
    '=':xs -> (xs, Equal currentLine)
    '<':'=':xs -> (xs, LessEqual currentLine)
    '<':xs -> (xs, Less currentLine)
    '>':'=':xs -> (xs, GreaterEqual currentLine)
    '>':xs -> (xs, Greater currentLine)
    '"':xs -> parseString xs currentLine
    '/':'/':xs -> (dropComment xs, Comment (takeComment xs) currentLine)
    '/':xs -> (xs, Slash currentLine)
    ' ':xs -> scanToken xs currentLine
    'O':'R':xs -> (xs, Or currentLine)
    'A':'N':'D':xs -> (xs, And currentLine)
    '\n':xs -> (xs, Newline currentLine)
    '\t':xs -> scanToken xs currentLine
    '\r':xs -> scanToken xs currentLine    
    unknown:xs -> if isDigit unknown 
                    then parseNumber token currentLine
                  else if isAlpha unknown
                    then parseIdentifierOrKeyword token currentLine
                    else (xs, Error ("Unexpected character '" ++ [unknown] ++ "'") currentLine)
    _ -> ("", EoF currentLine)

scan :: String -> [Result] -> Int -> [Result]
scan input tokens currentLine =
    case result of
        ([], EoF line) -> tokens ++ [EoF line]
        (remaining, Comment _ _) -> scan remaining tokens currentLine
        (remaining, Newline _) -> scan remaining tokens (currentLine + 1 )
        (remaining, token) -> scan remaining (tokens ++ [token]) currentLine
    where result = scanToken input currentLine

scanTokens :: String -> [Result]
scanTokens input =
    scan input [] 1