{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text                     as Text

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a) deriving Show

showStringTree :: BinaryTree Text.Text -> Text.Text
showStringTree Leaf = "Leaf"
showStringTree (Branch x y z) =
    "(Branch " <> showStringTree x <> " " <> y <> " " <> showStringTree z <> ")"

addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree Leaf i = Branch Leaf i Leaf
addElementToIntTree (Branch l x r) i
    | i < x  = Branch (addElementToIntTree l i) x r
    | i == x = Branch l x r
    | i > x  = Branch l x (addElementToIntTree r i)

doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist Leaf i = False
doesIntExist (Branch l x r) i | i == x    = True
                              | otherwise = doesIntExist l i || doesIntExist r i
