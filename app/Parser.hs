module Parser(parse, Statement(..), Expression(..)) where
import Scanner(Token(..), TokenType(..))

data Statement = 
    ExpressionStmt Expression |
    PrintStmt Expression |
    VarStmt TokenType Expression deriving (Show)

data Expression = 
    Binary Expression TokenType Expression |
    Assign TokenType Expression |
    Grouping Expression |
    Literal TokenType |
    Unary TokenType Expression
     deriving(Show)

parse :: [Token] -> [Statement]
parse tokens = do
    parseStatements $ map tokenType tokens
    
parseStatements :: [TokenType] -> [Statement]
parseStatements tokens =
    case tokens of
        [EoF] -> []
        [] -> [ExpressionStmt $ Literal $ Error "Unexpected end of file"]
        _ -> do
            let (rtokens, stmt) = statement tokens
            stmt : parseStatements rtokens

tokenType :: Token -> TokenType
tokenType (Token t _) = t

consumeSemiColon :: ([TokenType], Expression) -> ([TokenType], Expression)
consumeSemiColon (tokens, expr) = 
    case tokens of
        Semicolon:xss -> (xss, expr)
        a:_ -> ([], Literal $ Error $ "Expected to find ;. Got: " <> show a)
        [] -> ([], Literal $ Error $ "Expected to find ; Got: " <> [])

statement :: [TokenType] -> ([TokenType], Statement)
statement tokens =
    case tokens of
        Print:xs -> do
            let (rtokens, expre) = consumeSemiColon $ expression xs
            (rtokens, PrintStmt expre)
        Var:Identifier identifier:Equal:xs -> do
            let (rtokens, expre) = consumeSemiColon $ expression xs
            (rtokens, VarStmt (Identifier identifier) expre)
        _ -> do
            let (rtokens, expre) = consumeSemiColon $ expression tokens
            (rtokens, ExpressionStmt expre)


expression :: [TokenType] -> ([TokenType], Expression)
expression = assignment


mkAssign :: TokenType -> p -> (p -> (a, Expression)) -> (a, Expression)
mkAssign token tokens next =
    (rtokens, Assign token right)
    where 
        (rtokens, right) = next tokens
 

assignment :: [TokenType] -> ([TokenType], Expression)
assignment tokens =
    case tokens of
        Identifier name:Equal:xs -> mkAssign (Identifier name) xs assignment
        _ -> equality tokens

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
        Identifier name:xs -> (xs, Literal $ Identifier name)
        LeftParen:xs -> do
            let (rtokens, expr) = expression xs
            case rtokens of
                RightParen:xss -> (xss, Grouping expr)
                a:_ -> ([], Literal $ Error $ "Expected to find ) Got: " <> show a)
                [] -> ([], Literal $ Error $ "Expected to find ) Got: " <> [])
        a:_ -> ([], Literal $ Error $ "Expected to find literal. Got: " <> show a)
        [] -> ([], Literal $ Error $ "Expected to find literal. Got: " <> [])
