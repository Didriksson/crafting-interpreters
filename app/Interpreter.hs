module Interpreter(interpret) where
import Parser(Expression(..), Statement(..))
import Scanner(TokenType(..))
import Text.Read ( readMaybe )
import Data.Map (Map)
import qualified Data.Map as Map


data NilValue = NilValue deriving(Show)
newtype EvalError = EvalError String deriving(Show)
data Result = Value String | Void deriving(Show)

newtype State = State {
    environment :: Environment
} deriving(Show)

mkDefaultState :: State
mkDefaultState = State {
    environment = Map.empty
}

addVar :: State -> String -> String -> State
addVar s k v = s { environment = Map.insert k v $ environment s }

getVar :: State -> String -> Maybe String
getVar s k = Map.lookup k $ environment s

type Environment = Map String String

evalExpression :: State -> Expression -> Either EvalError (State, String)
evalExpression state (Literal l) =
    case l of
        NUMBER n -> Right (state, show n)
        STRING s -> Right (state, s)
        TRUE -> Right (state, show True)
        FALSE -> Right (state, show False)
        Nil -> Right (state, show NilValue)
        Identifier name -> case getVar state name of
                                Just v -> Right (state,v)
                                Nothing -> Left $ EvalError $ "Undefined variable: " <> name
        noLiteral -> Left $ EvalError $ "Not a literal: " <> show noLiteral

evalExpression state (Grouping expression) =
    evalExpression state expression

evalExpression state (Assign variable expression) =
    case variable of
        Identifier name -> do
            (_, result) <- evalExpression state expression
            let newstate = addVar state name result
            Right (newstate, show result)
        _ -> Left $ EvalError $ "Not a variable: " <> show variable


evalExpression state (Unary token expression) = do
    (_, right) <- evalExpression state expression
    case token of
        Minus -> case readMaybeNumber right of
                    Just n -> Right (state, show $ -1 * n)
                    Nothing -> Left $ EvalError $ "Expected number. Found: " <> show right
        Bang -> Right (state, show $ not $ readBoolean right)
        a -> Left $ EvalError $ "Not a unaryoperator: " <> show a

evalExpression state (Binary leftExp operator rightExp) = do
    (_, left) <- evalExpression state leftExp
    (_, right) <- evalExpression state rightExp
    case (readMaybeNumber left, readMaybeNumber right) of
        (Just ln,Just rn) -> 
            case operator of
                Minus -> Right (state, show $ ln - rn)
                Slash -> Right (state, show $ ln / rn)
                Star -> Right (state, show $ ln * rn)
                Plus -> Right (state, show $ ln + rn)
                a -> Left $ EvalError $ "Not a Binary operator: " <> show a
        (_,_) -> Right (state, left <> right)

readMaybeNumber :: String -> Maybe Float
readMaybeNumber mn =
    readMaybe mn :: Maybe Float

readBoolean :: String -> Bool
readBoolean s =
    read s :: Bool


eval :: Statement -> State -> IO (Either EvalError State)
eval (ExpressionStmt expression) state =
    case evalExpression state expression of
        Right (newstate, _) -> pure $ Right newstate
        Left err -> pure $ Left err

eval (PrintStmt expression) state = do
    let r = evalExpression state expression
    case r of
        Right (_, value) -> do 
            putStrLn value
            pure $ Right state
        Left err -> pure $ Left err

eval (VarStmt (Identifier varname) expr) state =
    case evalExpression state expr of
        Right (newstate, result) -> do
            pure $ Right $ addVar newstate varname result
        Left err -> pure $ Left err
eval (VarStmt _ _) _ =
    pure $ Left $ EvalError "Variable name must be an identifier"

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
    
