module Interpreter(interpret) where
import Parser(Expression(..))
import Scanner(TokenType(..))


data NilValue = NilValue deriving(Show)
newtype EvalError = EvalError String deriving(Show)

eval :: Expression -> Either String EvalError
eval (Literal l) =
    case l of
        NUMBER n -> Left $ show n
        STRING s -> Left $ show s
        TRUE -> Left $ show True
        FALSE -> Left $ show False
        Nil -> Left $ show NilValue
        noLiteral -> Right $ EvalError $ "Not a literal: " <> show noLiteral

eval (Grouping expression) =
    eval expression

eval (Unary token expression) =
    case token of
        Minus -> case right of
                    Left value -> Left $ show $ -1 * (read value :: Float)
                    Right err -> Right err
        Bang -> case evalBoolean (show token) of
            Left True -> Left $ show False
            Left False -> Left $ show  True
            Right err -> Right err

        a -> Right $ EvalError $ "Not a unaryoperator: " <> show a
    where
        right = eval expression

eval expression =
    Left $ show expression
    

evalBoolean :: String -> Either Bool EvalError
evalBoolean s =
    case s of
        "False" ->Left False
        "True" -> Left True
        a -> Right $ EvalError $ "Not a boolean: " <> show a




interpret :: Expression -> String
interpret expression =
     case eval expression of
        Left e -> e
        Right err -> show err

