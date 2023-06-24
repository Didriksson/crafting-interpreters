module Interpreter(interpret) where
import Parser(Expression(..))
import Scanner(TokenType(..))
import Text.Read


data NilValue = NilValue deriving(Show)
newtype EvalError = EvalError String deriving(Show)

eval :: Expression -> Either EvalError String
eval (Literal l) =
    case l of
        NUMBER n -> Right $ show n
        STRING s -> Right s
        TRUE -> Right $ show True
        FALSE -> Right $ show False
        Nil -> Right $ show NilValue
        noLiteral -> Left $ EvalError $ "Not a literal: " <> show noLiteral

eval (Grouping expression) =
    eval expression

eval (Unary token expression) = do
    right <- eval expression
    case token of
        Minus -> case readMaybeNumber right of
                    Just n -> Right $ show $ -1 * n
                    Nothing -> Left $ EvalError $ "Expected number. Found: " <> show right
        Bang -> Right $ show $ not $ readBoolean right
        a -> Left $ EvalError $ "Not a unaryoperator: " <> show a

eval (Binary leftExp operator rightExp) = do
    left <- eval leftExp
    right <- eval rightExp
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



interpret :: Expression -> String
interpret expression =
     case eval expression of
        Right e -> e
        Left err -> show err

