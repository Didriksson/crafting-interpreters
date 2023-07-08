module Interpreter(interpret) where
import Parser(Expression(..), Statement(..))
import Scanner(TokenType(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map


newtype EvalError = EvalError String deriving(Show)
data Result =  Stringtype String | Numbertype Float | Booltype Bool | NilValue

instance Num Result where
    (+) (Numbertype a) (Numbertype b) = Numbertype $ a + b
    (+) _ _ = error "Invalid operation"
    (*) (Numbertype a) (Numbertype b) = Numbertype $ a * b
    (*) _ _ = error "Invalid operation"
    (-) (Numbertype a) (Numbertype b) = Numbertype $ a - b
    (-) _ _ = error "Invalid operation"
    abs (Numbertype a) = Numbertype $ abs a
    abs _ = error "Invalid operation"
    signum (Numbertype a) = Numbertype $ signum a
    signum _ = error "Invalid operation"
    fromInteger a = Numbertype $ fromInteger a

instance Ord Result where
    compare (Numbertype a) (Numbertype b) = compare a b
    compare _ _ = error "Invalid operation"

instance Eq Result where
    (==) (Numbertype a) (Numbertype b) = a == b
    (==) (Stringtype a) (Stringtype b) = a == b
    (==) (Booltype a) (Booltype b) = a == b
    (==) NilValue NilValue = True
    (==) a b = error $ "Invalid operation == for " <> show a <> " and " <> show b

instance Show Result where
    show (Stringtype s) = s
    show (Numbertype n) = show n
    show (Booltype b) = show b
    show NilValue = "nil"

data State = State {
    environment :: Environment,
    enclosingEnvironment :: Maybe State
} deriving(Show)

mkDefaultState :: State
mkDefaultState = State {
    environment = Map.empty,
    enclosingEnvironment = Nothing
}

newEnvironment :: State -> State
newEnvironment s = s { environment = Map.empty, enclosingEnvironment = Just s }

popEnvironment :: State -> State
popEnvironment s = Data.Maybe.fromMaybe s (enclosingEnvironment s)

newVar :: State -> String -> Result -> State
newVar s k v = s { environment = Map.insert k v $ environment s }

assignVar :: State -> String -> Result -> Maybe State
assignVar s k v = 
    if existInCurrent then Just $ newVar s k v
    else case enclosingEnvironment s of
            Just e -> assignVar e k v
            Nothing -> Nothing
    where existInCurrent = Map.member k $ environment s

getVar :: State -> String -> Maybe Result
getVar s k = 
    case current of
        Just _ -> current
        Nothing -> case enclosingEnvironment s of
                        Just e -> getVar e k
                        Nothing -> Nothing
    where current = Map.lookup k $ environment s

type Environment = Map String Result

evalExpression :: State -> Expression -> Either EvalError (State, Result)
evalExpression state (Literal l) =
    case l of
        NUMBER n -> Right (state, Numbertype n)
        STRING s -> Right (state, Stringtype s)
        TRUE -> Right (state, Booltype True)
        FALSE -> Right (state, Booltype False)
        Nil -> Right (state, NilValue)
        Identifier name -> case getVar state name of
                                Just v -> Right (state, v)
                                Nothing -> Left $ EvalError $ "Undefined variable: " <> name
        noLiteral -> Left $ EvalError $ "Not a literal: " <> show noLiteral

evalExpression state (Grouping expression) =
    evalExpression state expression

evalExpression state (Assign variable expression) =
    case variable of
        Identifier name -> do
            (_, result) <- evalExpression state expression
            let newstate = assignVar state name result
            case newstate of
                Just s -> Right (s, result)
                Nothing -> Left $ EvalError $ "Undefined variable: " <> name            
        _ -> Left $ EvalError $ "Not a variable: " <> show variable


evalExpression state (Unary token expression) = do
    (_, right) <- evalExpression state expression
    case token of
        Minus -> Right (state, -1 * right)                    
        Bang -> case right of
                    Booltype b -> Right (state, Booltype $ not b)
                    _ -> Left $ EvalError $ "Expected boolean. Found: " <> show right
        a -> Left $ EvalError $ "Not a unaryoperator: " <> show a

evalExpression state (Binary leftExp operator rightExp) = do
    (_, left) <- evalExpression state leftExp
    (_, right) <- evalExpression state rightExp
    case operator of
        Greater -> Right (state, Booltype $ left > right)
        GreaterEqual -> Right (state, Booltype $ left >= right)
        Less -> Right (state, Booltype $ left < right)
        LessEqual -> Right (state, Booltype $ left <= right)                
        EqualEqual -> Right (state, Booltype $ left == right)
        BangEqual -> Right (state, Booltype $ left /= right)        
        _ -> case (left, right) of
                (Numbertype l, Numbertype r) -> case operator of
                                                    Plus -> Right (state, Numbertype $ l + r)
                                                    Minus -> Right (state, Numbertype $ l - r)
                                                    Star -> Right (state, Numbertype $ l * r)
                                                    Slash -> Right (state, Numbertype $ l / r)
                                                    _ -> Left $ EvalError $ "Not a binary operator: " <> show operator
                (l, r)                      -> case operator of
                                                    Plus -> Right (state, Stringtype $ show l <> show r)
                                                    _ -> Left $ EvalError $ "Not a binary operator: " <> show operator

evalExpression state (Logical leftExp operator rightExp) = do
    (newstate, left) <- evalExpression state leftExp
    let leftTruthy = truthiness left
    case operator of
        Or -> if leftTruthy
                then Right (newstate, left)
                else do    
                    (newstate', right) <- evalExpression state rightExp
                    let rightTruthy = truthiness right
                    if rightTruthy
                        then Right (newstate', right)
                        else Right (newstate', left)
        And -> if leftTruthy 
                then do                
                    (newstate', right) <- evalExpression state rightExp
                    let rightTruthy = truthiness right
                    if rightTruthy
                        then Right (newstate', right)
                        else Right (newstate', left)
                else Right (newstate, left)
        _ -> Left $ EvalError $ "Not a logical operator: " <> show operator

truthiness :: Result -> Bool
truthiness r =
    case r of 
        NilValue -> False
        Booltype b -> b
        _ -> True


eval :: Statement -> State -> IO (Either EvalError State)
eval (ExpressionStmt expression) state =
    case evalExpression state expression of
        Right (newstate, _) -> pure $ Right newstate
        Left err -> pure $ Left err

eval (PrintStmt expression) state = do
    let r = evalExpression state expression
    case r of
        Right (_, value) -> do 
            print value
            pure $ Right state
        Left err -> pure $ Left err

eval (VarStmt (Identifier varname) expr) state =
    case evalExpression state expr of
        Right (newstate, result) -> do
            pure $ Right $ newVar newstate varname result
        Left err -> pure $ Left err
eval (VarStmt _ _) _ =
    pure $ Left $ EvalError "Variable name must be an identifier"

eval (Block expressions) state = do
    evalResult <- evalAll expressions newstate  
    case evalResult of
        Right newState -> pure $ Right $ popEnvironment newState
        Left err -> pure $ Left err
    where 
        newstate = newEnvironment state        

eval (IfStmt conditional thenBranch mElseBranch) state = do
    let evalConditional = evalExpression state conditional
    case evalConditional of
        Left err -> pure $ Left err
        Right (state', result) ->
            case result of
                Booltype b ->if b
                                then eval thenBranch state'
                            else 
                                case mElseBranch of
                                    Just elseBranch -> eval elseBranch state'
                                    Nothing -> pure $ Right state'
                _ -> pure $ Left $ EvalError "Expected boolean expression"


eval (WhileStmt conditional body) state = do
    let evalConditional = evalExpression state conditional
    case evalConditional of
        Left err -> pure $ Left err
        Right (state', result) ->
            case result of
                Booltype b -> if b
                                then do
                                    evalResult <- eval body state'
                                    case evalResult of
                                        Right newState -> eval (WhileStmt conditional body) newState
                                        Left err -> pure $ Left err
                                else pure $ Right state'
                _ -> pure $ Left $ EvalError "Expected boolean expression"

eval (ForStmt mInit mCondition mIncrement body) state = do
    initState <- case mInit of
                    Just init' -> do eval init' state
                    Nothing -> pure $ Right state
    case initState of
        Right state' -> do
            case mCondition of
                Just cond -> do
                    let condEvalResult = evalExpression state' cond
                    case condEvalResult of
                        Right (condState, condResult) -> do
                            case condResult of
                                Booltype True -> do
                                    bodyeval <- eval body condState
                                    case bodyeval of
                                        Right newstate -> do
                                            let incrementEvalResult = case mIncrement of
                                                                        Just inc -> evalExpression newstate inc
                                                                        Nothing -> Right (newstate, NilValue)    
                                            case incrementEvalResult of
                                                Right (incrementState, _) -> eval (ForStmt Nothing mCondition mIncrement body) incrementState
                                                Left err -> pure $ Left err
                                        Left err -> pure $ Left err
                                Booltype False -> pure $ Right condState
                                _ -> pure $ Left $ EvalError "Expected boolean expression"
                        Left err -> pure $ Left err
                Nothing -> do
                    bodyeval <- eval body state'
                    case bodyeval of
                        Right newstate -> do
                            let incrementEvalResult = case mIncrement of
                                                        Just inc -> evalExpression newstate inc
                                                        Nothing -> Right (newstate, NilValue)    
                            case incrementEvalResult of
                                Right (incrementState, _) -> eval (ForStmt Nothing mCondition mIncrement body) incrementState
                                Left err -> pure $ Left err
                        Left err -> pure $ Left err
        Left err -> pure $ Left err
    


evalAll :: [Statement] -> State -> IO (Either EvalError State)
evalAll [] state = pure $ Right state
evalAll (s:ss) state = do
    evalResult <- eval s state        
    case evalResult of
        Right newState -> evalAll ss newState
        Left err -> pure $ Left err


-- Interprets all statements using eval and returns the final state
interpret :: [Statement] -> IO (Either EvalError State)
interpret statements = do
    let state = mkDefaultState
    evalAll statements state
    
