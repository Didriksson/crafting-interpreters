module Interpreter(interpret) where
import Parser(Expression(..), Statement(..))
import Scanner(TokenType(..))
import Text.Read


data NilValue = NilValue deriving(Show)
newtype EvalError = EvalError String deriving(Show)
data Result = Value String | Void deriving(Show)

evalExpression :: Expression -> Either EvalError String
evalExpression (Literal l) =
    case l of
        NUMBER n -> Right $ show n
        STRING s -> Right s
        TRUE -> Right $ show True
        FALSE -> Right $ show False
        Nil -> Right $ show NilValue
        noLiteral -> Left $ EvalError $ "Not a literal: " <> show noLiteral

evalExpression (Grouping expression) =
    evalExpression expression

evalExpression (Unary token expression) = do
    right <- evalExpression expression
    case token of
        Minus -> case readMaybeNumber right of
                    Just n -> Right $ show $ -1 * n
                    Nothing -> Left $ EvalError $ "Expected number. Found: " <> show right
        Bang -> Right $ show $ not $ readBoolean right
        a -> Left $ EvalError $ "Not a unaryoperator: " <> show a

evalExpression (Binary leftExp operator rightExp) = do
    left <- evalExpression leftExp
    right <- evalExpression rightExp
    case (readMaybeNumber left, readMaybeNumber right) of
        (Just ln,Just rn) -> 
            case operator of
                Minus -> Right $ show $ ln - rn
                Slash -> Right $ show $ ln / rn
                Star -> Right $ show $ ln * rn
                Plus -> Right $ show $ ln + rn
                a -> Left $ EvalError $ "Not a Binary operator: " <> show a
        (_,_) -> Right $ left <> right

readMaybeNumber :: String -> Maybe Float
readMaybeNumber mn =
    readMaybe mn :: Maybe Float

readBoolean :: String -> Bool
readBoolean s =
    read s :: Bool



eval :: Statement -> IO (Either EvalError Result)
eval (ExpressionStmt expression) =
    case evalExpression expression of
        Right value -> pure $ Right $ Value value
        Left err -> pure $ Left err

eval (PrintStmt expression) = do
    let result = evalExpression expression
    case result of
        Right value -> do 
            putStrLn value
            pure $ Right Void
        Left err -> pure $ Left err


-- Iterating over all statements and prints PrintStmts and evalExpression for ExpressionStmts
interpret :: [Statement] -> IO (Either EvalError [Result])
interpret statements = do
    eitherEvaluated <- mapM eval statements
    let result = sequence eitherEvaluated
    return result