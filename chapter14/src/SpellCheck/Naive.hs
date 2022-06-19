module SpellCheck.Naive where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

editDistance :: Text -> Text -> Int
editDistance stringA stringB
    | T.null stringA                   = T.length stringB
    | T.null stringB                   = T.length stringA
    | T.head stringA == T.head stringB = editDistance restOfA restOfB
    | otherwise = 1 + minimum [insertCost, deleteCost, swapCost]
  where
    restOfA    = T.tail stringA
    restOfB    = T.tail stringB
    deleteCost = editDistance restOfA stringB
    insertCost = editDistance stringA restOfB
    swapCost   = editDistance restOfA restOfB
