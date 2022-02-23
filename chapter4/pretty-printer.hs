{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text                     as Text

data Expr = Lit Int
    | Sub Expr Expr
    | Add Expr Expr
    | Mul Expr Expr
    | Div Expr Expr

prettyPrint :: Expr -> Text.Text
prettyPrint (Lit i    ) = Text.pack . show $ i
prettyPrint (Sub e1 e2) = "(" <> prettyPrint e1 <> "-" <> prettyPrint e2 <> ")"
prettyPrint (Add e1 e2) = "(" <> prettyPrint e1 <> "+" <> prettyPrint e2 <> ")"
prettyPrint (Mul e1 e2) = "(" <> prettyPrint e1 <> "*" <> prettyPrint e2 <> ")"
prettyPrint (Div e1 e2) = "(" <> prettyPrint e1 <> "/" <> prettyPrint e2 <> ")"
