{-# LANGUAGE TemplateHaskell, RecordWildCards, DuplicateRecordFields, NamedFieldPuns #-}
module Dominion.Effects where
import Dominion.Types

activateCardEffect :: Board -> CardEffect -> Board
activateCardEffect board (CoinValue x) = do
  let state' = board ^. state
  let money' = (state' ^. money) + x
  board{_state=state'{_money=money'}}
activateCardEffect board _ = board