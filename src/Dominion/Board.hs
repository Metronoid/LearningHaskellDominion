{-# LANGUAGE TemplateHaskell, RecordWildCards, DuplicateRecordFields, NamedFieldPuns #-}
module Dominion.Board where

import Lib.Array
import Data.List
import Data.Maybe
import System.Random
import Control.Lens

import Dominion.Types
import Dominion.Cards as Cards
import Dominion.Player as Player
import Dominion.Effects as Effects
import Dominion.Types as T

gen = mkStdGen 123

switchPlayer :: Player -> [Player] -> [Player]
switchPlayer y (x:xs) = xs ++ [y]

getMoney :: [Card] -> State -> Int
getMoney hand state = do
  let money = map coinValue (concatMap T._effect hand)
  if state ^. T.merch > 0 then do
    let silver = findCardByName "Silver" hand
    if isJust silver then
      (state ^. T.merch) + (sum money)
    else
      sum money
  else
    sum money
  where
    coinValue (T.CoinValue x) = x
    coinValue _ = 0

playCard :: Board -> [CardEffect] -> IO (Board)
playCard board (x:xs) = do
  board' <- (activateCardEffect board x)
  playCard board' xs
playCard board [] = return board

-- playAction :: [[CardEffect]] -> []

checkGameEnding :: Board -> Bool
checkGameEnding board@Board{..} = do
  let province = (findBuyableCardsByName "Province" _buyList)!!0
  (province ^. T.stock) == 0

setupPlayer :: Player -> Player
setupPlayer player = do
  drawCards player {_deck = shuffleDeck (player ^. deck)} 5

setup = do
  let buyList' = [  BuyableCard {_card= copper, _stock = 10}, 
              BuyableCard {_card= silver, _stock = 10}, 
              BuyableCard {_card= gold, _stock = 10},
              BuyableCard {_card= curse, _stock = 10},
              BuyableCard {_card= estate, _stock = 10},
              BuyableCard {_card= duchy, _stock = 10},
              BuyableCard {_card= province, _stock = 10},
              BuyableCard {_card= cellar, _stock = 10},
              BuyableCard {_card= market, _stock = 10},
              BuyableCard {_card= smithy, _stock = 10},
              BuyableCard {_card= village, _stock = 10},
              BuyableCard {_card= merchant, _stock = 10},
              BuyableCard {_card= mine, _stock = 10}]

  let blue' = setupPlayer blue
  let red' = setupPlayer red

  let players = [blue', red']

  let board = Board {_buyList = buyList', _players=players, _state=baseState}

  nextTurn board

nextTurn :: Board -> IO ()
nextTurn board = do 
  putStrLn "-------------------------------"

  if checkGameEnding board then
    displayGameResults (board ^. players)
  else do
    let player = (board ^. players)!!0
    let hand' = player ^. hand
    putStrLn $ (player ^. T.playerName) ++ "'s turn."
    showHand hand'

    board' <- playActions board{_state=baseState}
    let state = board' ^. T.state
    let player' = (board' ^. T.players)!!0
    putStrLn $ show $ state ^. T.money
    buyPhase board'{_state=state{_money=(state ^. T.money + (getMoney (player' ^. T.hand) state))}}

playActions :: Board -> IO Board
playActions board = do
  let player = (board ^. players)!!0
  let hand = player ^. T.hand
  let cards = filter (isCardType Action) hand
  if length cards > 0 then do
    let uses = board ^. state ^. actions 
    if uses > 0 then do
      putStrLn $ "You have " ++ show uses ++ " action points left to use."
      showHand cards
      showHand $ player ^. T.deck
      putStrLn $ "Which card do you want to play?"
      response <- getLine
      let card = findCardByName response cards
      if isJust card then do
        let hand' = removeCard (fromJust card) hand
        let players' = replacePlayer player{_hand=hand'} (board ^. T.players)
        board' <- playCard board{_players=players'} (fromJust card ^. effect)
        playActions board'
      else do
        putStrLn $ "Couldn't find the card."
        return board
    else do 
      putStrLn $ "You have no actions left to use."
      return board
  else do
    putStrLn $ "There are no action cards you can use."
    return board

buyPhase :: Board -> IO ()
buyPhase board = do
  let player = (board ^. players)!!0
  let state' = board ^. T.state
  if (state' ^. buys > 0) then do
    let buyList' = canBuy (board ^. buyList) (state' ^. T.money)
    putStrLn ("You can buy: " ++ (intercalate ", " (map show buyList')))
    putStrLn ("You have " ++ show (state' ^. money) ++ " money left")
    putStrLn ("You have " ++ show (state' ^. buys) ++ " buy actions left")
    putStrLn "What do you want to buy?"
    buyRequest <- getLine
    let boughtCard = buyCard buyRequest (board ^. buyList)
    if isJust boughtCard then do
      let card = fromJust boughtCard
      putStrLn ("You have bought an " ++ (card ^. T.cardName))
      let state'' = state' {_money=(state' ^. money - (card ^. T.cost)), _buys = state' ^. buys-1}
      let player' = player {_discard = ([card] ++ (player ^. discard))}
      let players' = replacePlayer player' (board ^. players)

      let elemCard = elemIndex card (map (T._card) (board ^. buyList))
      if isNothing elemCard then
        buyPhase board {_players=players', _state=state''}
      else do
        let buyList' = replaceNth (fromJust elemCard) (lowerStock((board ^. buyList)!!(fromJust elemCard))) (board ^. buyList)
        buyPhase board {_players=players', _buyList = buyList', _state=state''}
    else do
      putStrLn ("You decided not to use your buy.")
      let player' = drawCards (discardHand player) 5
      let players' = switchPlayer player' (board ^. players)
      nextTurn board{_players=players'}
  else do
    putStrLn ("You were not able to buy anything else.")
    let player' = drawCards (discardHand player) 5
    let players' = switchPlayer player' (board ^. players)
    nextTurn board{_players=players'}

displayGameResults :: [Player] -> IO()
displayGameResults [] = putStrLn "Thanks for Playing Dominion with me"
displayGameResults players = do
  let (player, players') = splitFrom 0 0 players
  let player' = player!!0
  putStrLn (show (player' ^. T.playerName) ++ ": has ended with " ++ show (getPlayerScore (player')) ++ " points")
  displayGameResults players'


