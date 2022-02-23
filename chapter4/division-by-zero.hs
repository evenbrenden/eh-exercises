{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text                     as Text

data Expr = Lit Int
    | Sub Expr Expr
    | Add Expr Expr
    | Mul Expr Expr
    | Div Expr Expr

eval :: Expr -> Either Text.Text Int
eval expr = case expr of
    Lit num          -> Right num
    Add arg1 arg2    -> eval' (+) arg1 arg2
    Sub arg1 arg2    -> eval' (-) arg1 arg2
    Mul arg1 arg2    -> eval' (*) arg1 arg2
    Div arg1 (Lit 0) -> Left "Division by zero"
    Div arg1 arg2    -> eval' div arg1 arg2
  where
    eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Either Text.Text Int
    eval' operator arg1 arg2 =
        let (Right e1) = eval arg1
            (Right e2) = eval arg2
        in  Right $ operator e1 e2
