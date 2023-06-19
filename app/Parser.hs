module Parser(syntaxTree) where

data Expression = 
    Binary Expression Token Expression |
    Grouping Expression |
    Literal Token |
    Unary Token Expression

astPrinter 
syntaxTree 
