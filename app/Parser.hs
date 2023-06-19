module Parser() where
import Scanner(Token)

data Expression = 
    Binary Expression Token Expression |
    Grouping Expression |
    Literal Token |
    Unary Token Expression

expression :: [Token] -> Expression
expression = equality



equality :: [Token] -> Expression
equality tokens = do
    let (exp, rtokens) = comparison tokens
    case rtokens of
        BangEqual:xs -> Binary exp BangEqual $ comparison xs
        EqualEqual:xs -> Binary exp EqualEqual $ comparison xs
        _ -> exp

mkBinary :: Expression -> Token -> [Token] -> ([Token], Expression)
mkBinary exp token tokens =
    (rtokens, Binary exp GreaterEqual right)
    where 
        (right, rtokens) = term xs

comparison :: [Token] -> ([Token], Expression)
comparison tokens = do
    let (exp, rtokens) = term tokens
    case rtokens of
        Greater:xs -> Binary exp Greater $ term xs
        GreaterEqual:xs -> mkBinary exp GreaterEqual xs
        Less:xs -> mkBinary exp Less xs
        LessEqual:xs -> mkBinary exp LessEqual xs
        _ -> (exp, rtokens)


