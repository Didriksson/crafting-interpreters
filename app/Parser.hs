module Parser(parse, Expression(..)) where
import Scanner(Token(..), TokenType(..))

data Expression = 
    Binary Expression TokenType Expression |
    Grouping Expression |
    Literal TokenType |
    Unary TokenType Expression deriving(Show)

parse :: [Token] -> Expression
parse tokens =
    snd $ expression $ map tokenType tokens

tokenType :: Token -> TokenType
tokenType (Token t _) = t

expression :: [TokenType] -> ([TokenType], Expression)
expression = equality

equality :: [TokenType] -> ([TokenType], Expression)
equality tokens =
    case rtokens of
        BangEqual:xs -> mkBinary expre BangEqual xs comparison
        EqualEqual:xs -> mkBinary expre EqualEqual xs comparison
        _ -> (rtokens, expre)
    where 
        (rtokens, expre) = comparison tokens

mkBinary :: Expression -> TokenType -> p -> (p -> (a, Expression)) -> (a, Expression)
mkBinary expr token tokens next =
    (rtokens, Binary expr token right)
    where 
        (rtokens, right) = next tokens

mkUnary :: TokenType -> p -> (p -> (a, Expression)) -> (a, Expression)
mkUnary token tokens next = 
    (rtokens, Unary token right)
    where (rtokens, right) = next tokens

comparison :: [TokenType] -> ([TokenType], Expression)
comparison tokens = do
    let (rtokens, expr) = term tokens
    case rtokens of
        Greater:xs -> mkBinary expr Greater xs term
        GreaterEqual:xs -> mkBinary expr GreaterEqual xs term
        Less:xs -> mkBinary expr Less xs term
        LessEqual:xs -> mkBinary expr LessEqual xs term
        _ -> (rtokens, expr)


term :: [TokenType] -> ([TokenType], Expression)
term tokens = do
    let (rtokens, expr) = factor tokens
    case rtokens of
        Minus:xs -> mkBinary expr Minus xs factor
        Plus:xs -> mkBinary expr Plus xs factor
        _ -> (rtokens, expr)
 
factor :: [TokenType] -> ([TokenType], Expression)
factor tokens = do
    let (rtokens, expr) = unary tokens
    case rtokens of
        Slash:xs -> mkBinary expr Slash xs unary
        Star:xs -> mkBinary expr Star xs unary
        _ -> (rtokens, expr)

unary :: [TokenType] -> ([TokenType], Expression)
unary tokens = do    
    case tokens of
        Bang:xs -> mkUnary Bang xs unary
        Minus:xs -> mkUnary Minus xs unary
        _ -> primary tokens

primary :: [TokenType] -> ([TokenType], Expression)
primary tokens = do    
    case tokens of
        FALSE:xs -> (xs, Literal FALSE)
        TRUE:xs -> (xs, Literal TRUE)
        Nil:xs -> (xs, Literal Nil)
        NUMBER n:xs -> (xs, Literal $ NUMBER n)
        STRING n:xs -> (xs, Literal $ STRING n)
        LeftParen:xs -> do
            let (rtokens, expr) = expression xs
            case rtokens of
                RightParen:xss -> (xss, Grouping expr)
                a:_ -> ([], Literal $ Error $ "Exoected to find ). Got: " <> show a)
                [] -> ([], Literal $ Error $ "Exoected to find ). Got: " <> [])
        a:_ -> ([], Literal $ Error $ "Expected to find literal. Got: " <> show a)
        [] -> ([], Literal $ Error $ "Expected to find literal. Got: " <> [])
