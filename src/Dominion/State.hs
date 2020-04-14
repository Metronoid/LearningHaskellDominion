{-# LANGUAGE TemplateHaskell, RecordWildCards, DuplicateRecordFields, NamedFieldPuns #-}
module Dominion.State where
import Control.Lens
import Dominion.Types

baseState = State {_money = 0, _actions = 1, _buys = 1}

combineStates :: State -> State -> State
combineStates (State money actions buys) (State addMoney addActions addBuys) =
    State{_money=money+addMoney, _actions=actions+addActions, _buys=buys+addBuys}

sumStates :: [State] -> State
sumStates [] = State {_money = 0, _actions = 1, _buys = 1}
sumStates (x:xs) = combineStates x (sumStates xs)