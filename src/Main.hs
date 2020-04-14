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

askTime = do
  putStrLn "How many seconds would like to give the AI per turn? (Default: 10.0)"
  response <- getLine
  case response of
      "" -> return 10.0
      s -> case readMaybe s of
          Just n -> return n
          Nothing -> askTime

main = do
  first <- askFirst
  time <- askTime
  setup