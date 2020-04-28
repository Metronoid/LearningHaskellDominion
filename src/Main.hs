module Main where

import Dominion.Board
import Text.Read

askFirst = do
    putStrLn "Do you want to go first (1) or second (2)"
    response <- getLine
    case response of
        "1" -> return $ Just True
        "2" -> return $ Just False
        _ -> askFirst

main = do
  first <- askFirst
  setup