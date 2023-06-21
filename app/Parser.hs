module Parser(parse) where
import Scanner(Token(..), TokenType(..))

data Expression = 
    Binary Expression TokenType Expression |
    Grouping Expression |
    Literal TokenType |
    Unary TokenType Expression deriving(Show)

parse :: [Token] -> Expression
parse tokens =
    expression $ map tokenType tokens

tokenType :: Token -> TokenType
tokenType (Token t _) = t

expression :: [TokenType] -> Expression
expression = equality

equality :: [TokenType] -> Expression
equality tokens =
    case rtokens of
        BangEqual:xs -> Binary expre BangEqual $ snd $ comparison xs
        EqualEqual:xs -> Binary expre EqualEqual $ snd $ comparison xs
        _ -> expre
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
        _ -> ([], primary tokens)

primary :: [TokenType] -> Expression
primary tokens = do    
    case tokens of
        FALSE:_ -> Literal FALSE
        TRUE:_ -> Literal TRUE
        Nil:_ -> Literal Nil
        NUMBER n:_ -> Literal $ NUMBER n
        STRING n:_ -> Literal $ STRING n
        RightParen:xs -> do
            let expr = expression xs
            Grouping expr
        a -> Literal $ Error $ "Förväntade mig att hitta en literal. Fick: " <> show a
