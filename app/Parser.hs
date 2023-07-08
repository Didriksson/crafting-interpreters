module Parser(parse, Statement(..), Expression(..)) where
import Scanner(Token(..), TokenType(..))

data Statement = 
    ExpressionStmt Expression |
    PrintStmt Expression |
    Block [Statement] |
    IfStmt Expression Statement (Maybe Statement) |
    WhileStmt Expression Statement |
    ForStmt (Maybe Statement) (Maybe Expression) (Maybe Expression) Statement |
    VarStmt TokenType Expression deriving (Show)

data Expression = 
    Binary Expression TokenType Expression |
    Assign TokenType Expression |
    Grouping Expression |
    Literal TokenType |
    Logical Expression TokenType Expression |
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

consumeSemicolon :: ([TokenType], Expression) -> ([TokenType], Expression)
consumeSemicolon ([], _) = ([], Literal $ Error $ "Expected to find " <> show Semicolon <> ". Got nothing.")
consumeSemicolon ([EoF], _) = ([], Literal $ Error $ "Expected to find " <> show Semicolon <> ". Got: end of file.")
consumeSemicolon (tokens, expr) 
    | x == Semicolon = (xs, expr)
    | otherwise =  ([], Literal $ Error $ "Expected to find " <> show Semicolon <> ". Got: " <> show x)
    where (x:xs) = tokens

statement :: [TokenType] -> ([TokenType], Statement)
statement tokens =
    case tokens of
        Print:xs -> do
            let (rtokens, expre) = consumeSemicolon $ expression xs
            (rtokens, PrintStmt expre)
        Var:Identifier identifier:Equal:xs -> do
            let (rtokens, expre) = consumeSemicolon $ expression xs
            (rtokens, VarStmt (Identifier identifier) expre)
        LeftBrace:xs -> block xs
        If:xs -> ifStatement xs
        For:xs -> forStatement xs
        While:xs -> whileStatement xs
        _ -> do
            let (rtokens, expre) = consumeSemicolon $ expression tokens
            (rtokens, ExpressionStmt expre)


parseInitialiser :: [TokenType] -> ([TokenType], Maybe Statement)
parseInitialiser tokens =
    case tokens of
        LeftParen:Var:Identifier identifier:Equal:xs -> do
                        let (rtokens, expre) = consumeSemicolon $ expression xs
                        (rtokens, Just $ VarStmt (Identifier identifier) expre)
        LeftParen:Semicolon:xs -> (xs, Nothing)
        LeftParen:xs -> do
                        let (rtokens, expre) = expression xs
                        (rtokens, Just $ ExpressionStmt expre)

        _ -> ([], Just $ ExpressionStmt $ Literal $ Error "Expected to find (")

parseCondition :: [TokenType] -> ([TokenType], Maybe Expression)
parseCondition tokens =
    case tokens of
        Semicolon:xs -> (xs, Nothing)
        _ -> do
            let (rtokens, expre) = consumeSemicolon $ expression tokens
            (rtokens, Just expre)

parseIncrement :: [TokenType] -> ([TokenType], Maybe Expression)
parseIncrement tokens =
    case tokens of
        RightParen:xs -> (xs, Nothing)
        _ -> do
            let (rtokens, expre) = expression tokens
            case rtokens of
                RightParen:ys -> (ys, Just expre)
                _ -> ([], Just $ Literal $ Error "Expected to find )")

forStatement :: [TokenType] -> ([TokenType], Statement)
forStatement tokens = do
    (rtokens''', ForStmt initialiser condition increment body)
    where         
        (rtokens, initialiser) = parseInitialiser tokens
        (rtokens', condition) = parseCondition rtokens
        (rtokens'', increment) = parseIncrement rtokens'
        (rtokens''', body) =  statement rtokens''
   

whileStatement :: [TokenType] -> ([TokenType], Statement)
whileStatement tokens = 
    case tokens of
        LeftParen:xs -> do
            let (rtokens, condition ) = expression xs
            case rtokens of
                RightParen:ys -> do
                    let (rtokens', body) = statement ys
                    (rtokens', WhileStmt condition body)
                _ -> ([], ExpressionStmt $ Literal $ Error "Expected to find )")
        _ -> ([], ExpressionStmt $ Literal $ Error "Expected to find (")

ifStatement :: [TokenType] -> ([TokenType], Statement)
ifStatement tokens = 
    case tokens of
        LeftParen:xs -> do
            let (rtokens, condition ) = expression xs
            case rtokens of
                RightParen:ys -> do
                    let (rtokens', thenBranch) = statement ys
                    case rtokens' of
                        Else:zs -> do
                            let (rtokens'', elseBranch) = statement zs
                            (rtokens'', IfStmt condition thenBranch $ Just elseBranch)
                        _ -> (rtokens', IfStmt condition thenBranch Nothing) 
                _ -> ([], ExpressionStmt $ Literal $ Error "Expected to find )")
        _ -> ([], ExpressionStmt $ Literal $ Error "Expected to find (")



parseUntilClosingBlock :: [TokenType] -> ([TokenType], [Statement])
parseUntilClosingBlock tokens =
    case tokens of
        RightBrace:xs -> (xs, [])
        _ -> do
            let (rtokens, stmt) = statement tokens
            let (rtokens', stmts) = parseUntilClosingBlock rtokens
            (rtokens', stmt : stmts)


block :: [TokenType] -> ([TokenType], Statement)
block tokens =   
    (rtokens, Block parsedBlock) 
    where 
        (rtokens, parsedBlock) = parseUntilClosingBlock tokens


expression :: [TokenType] -> ([TokenType], Expression)
expression = assignment


mkAssign :: TokenType -> p -> (p -> (a, Expression)) -> (a, Expression)
mkAssign token tokens next =
    (rtokens, Assign token right)
    where 
        (rtokens, right) = next tokens


mkLogical :: Expression -> TokenType -> p -> (p -> (a, Expression)) -> (a, Expression)
mkLogical expr operator tokens next =
    (rtokens, Logical expr operator right)
    where 
        (rtokens, right) = next tokens
 

assignment :: [TokenType] -> ([TokenType], Expression)
assignment tokens =
    case tokens of
        Identifier name:Equal:xs -> mkAssign (Identifier name) xs assignment
        _ -> logicOr tokens

logicOr :: [TokenType] -> ([TokenType], Expression)
logicOr tokens = 
    case rtokens of
        Or:xs -> mkLogical left Or xs logicAnd
        _ -> (rtokens, left)
    where (rtokens, left) = logicAnd tokens


logicAnd :: [TokenType] -> ([TokenType], Expression)
logicAnd tokens = 
    case rtokens of
        And:xs -> mkLogical left And xs equality
        _ -> (rtokens, left)
    where (rtokens, left) = equality tokens


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
