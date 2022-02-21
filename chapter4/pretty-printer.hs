data Expr = Lit Int
    | Sub Expr Expr
    | Add Expr Expr
    | Mul Expr Expr
    | Div Expr Expr

prettyPrint :: Expr -> String
prettyPrint (Lit i    ) = show i
prettyPrint (Sub e1 e2) = "(" <> prettyPrint e1 <> "-" <> prettyPrint e2 <> ")"
prettyPrint (Add e1 e2) = "(" <> prettyPrint e1 <> "+" <> prettyPrint e2 <> ")"
prettyPrint (Mul e1 e2) = "(" <> prettyPrint e1 <> "*" <> prettyPrint e2 <> ")"
prettyPrint (Div e1 e2) = "(" <> prettyPrint e1 <> "/" <> prettyPrint e2 <> ")"
