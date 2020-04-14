{-# LANGUAGE TemplateHaskell, RecordWildCards, DuplicateRecordFields, NamedFieldPuns #-}
module Dominion.Board where

import Lib.Array
import Data.List
import Data.Maybe
import System.Random
import Control.Lens

import Dominion.Types
import Dominion.Cards as Cards
import Dominion.State as State
import Dominion.Player as Player
import Dominion.Types as T

gen = mkStdGen 123

canBuy :: [BuyableCard] -> Int -> [BuyableCard]
canBuy cards money = concatMap (listBuy money) cards

drawHand :: Player -> Int -> ([Card], [Card], [Card])
drawHand player amm = do
  let deck' = player ^. deck
  if length deck' >= amm then do
    let (hand, deck'') = splitFrom 0 (amm-1) deck'
    (hand, deck'', player ^. discard)
  else do
    let discard' = player ^. discard
    let (hand, deck'') = splitFrom 0 (amm - length deck' - 1) (shuffleDeck discard')
    (deck' ++ hand, deck'', [])


showHand :: [Card] -> IO ()
showHand hand = do
  let cards = map (\x -> T._cardName x) hand
  putStrLn ("Hand: " ++ (intercalate " " cards))

switchPlayer :: Player -> [Player] -> [Player]
switchPlayer y (x:xs) = xs ++ [y]

replacePlayer :: Player -> [Player] -> [Player]
replacePlayer y (x:xs) = [y] ++ xs


getMoney :: [Card] -> Int
getMoney hand = do
  let money = map coinValue (concatMap T._effect hand)
  sum money
  where
    coinValue (T.CoinValue x) = x
    coinValue _ = 0

getActions :: [Card] -> [Card]
getActions hand = filter isAction hand

-- playCard :: Board -> Card -> Board
-- playCard board card = do
--   map (trigger board) (card ^. effect)
--   where trigger board effect = 

-- playAction :: [[CardEffect]] -> []

checkGameEnding :: Board -> Bool
checkGameEnding board@Board{..} = do
  let province = (findBuyableCardsByName "Province" _buyList)!!0
  (province ^. T.stock) == 0
  
shuffleDeck :: [Card] -> [Card]
shuffleDeck deck = shuffle' deck gen

setupPlayer :: Player -> Player
setupPlayer player = do
  let player' = player {_deck = shuffleDeck (player ^. deck)}
  let (hand', deck', discard') = drawHand player' 5
  player{_deck=deck', _discard=discard', _hand=hand'}

setup = do
  let buyList' = [  BuyableCard {_card= copper, _stock = 10}, 
              BuyableCard {_card= silver, _stock = 10}, 
              BuyableCard {_card= gold, _stock = 10},
              BuyableCard {_card= curse, _stock = 10},
              BuyableCard {_card= estate, _stock = 10},
              BuyableCard {_card= duchy, _stock = 10},
              BuyableCard {_card= province, _stock = 10}]

  let blue' = setupPlayer blue
  let red' = setupPlayer red

  let players = [blue', red']

  let board = Board {_buyList = buyList', _players=players, _state=baseState}

  nextTurn board

nextTurn :: Board -> IO ()
nextTurn board = do 
  putStrLn "-------------------------------"

  let player = (board ^. players)!!0
  let hand' = player ^. hand
  putStrLn $ ((player ^. T.playerName) ++ "'s turn.")
  showHand hand'

  let money = getMoney hand'
  let state = baseState {_money=money}

  actionPhase board{_state=state}

actionPhase :: Board -> IO ()
actionPhase board = do
  let player = (board ^. players)!!0
  let hand' = player ^. hand
  let actions = getActions hand'

  -- if length actions > 0 then

  -- else
  playActions board actions
  
  let (hand'', deck', discard') = drawHand player 5
  let player' = player {_deck=deck', _discard=(hand' ++ discard'), _hand=hand''}
  let players' = replacePlayer player' (board ^. players)

  buyPhase board{_players=players'}

playActions :: Board -> [Card] -> IO ()
playActions board actions = do
  if length actions > 0 then do
    showHand actions
    putStrLn $ ("Which card do you want to play?")
    response <- getLine
    let card = findCardByName response actions
    if isJust card then do
      -- let board' = triggerCardEffect board card
      let actions' = removeItem (fromJust card) actions
      playActions board actions'
    else putStrLn $ ("There are no actions you can play.")
  else
    putStrLn $ ("There are no actions you can play.")
  -- let effects = card ^. Cards.effect
  -- let states = map activateEffect effects
  -- sumStates states

buyPhase :: Board -> IO ()
buyPhase board = do
  if checkGameEnding board then
    displayGameResults (board ^. players)
  else do
    let player = (board ^. players)!!0
    let state' = board ^. state
    if (state' ^. buys > 0) then do
      let buyList' = canBuy (board ^. buyList) (state' ^. T.money)
      putStrLn ("You can buy: " ++ (intercalate ", " (map show buyList')))
      putStrLn ("You have " ++ show (state' ^. money) ++ " money left")
      putStrLn ("You have " ++ show (state' ^. buys) ++ " buy actions left")
      putStrLn "What do you want to buy?"
      buyRequest <- getLine
      let boughtCard = buyCard buyRequest (board ^. buyList)
      if (boughtCard ^. T.cardName) == "Empty" then do
        putStrLn ("You decided not to use your buy.")
        let players' = switchPlayer player (board ^. players)
        nextTurn board{_players=players'}
      else do
        putStrLn ("You have bought an " ++ (boughtCard ^. T.cardName))
        let state'' = State {_money=(state' ^. money - (boughtCard ^. T.cost)), _actions=state' ^. actions, _buys = state' ^. buys-1}
        let player' = player {_discard = ([boughtCard] ++ (player ^. discard))}
        let players' = replacePlayer player' (board ^. players)

        let elemCard = elemIndex boughtCard (map (^. card) (board ^. buyList))
        if isNothing elemCard then
          buyPhase board {_players=players', _state=state''}
        else do
          let buyList' = replaceNth (fromJust elemCard) (lowerStock((board ^. buyList)!!(fromJust elemCard))) (board ^. buyList)
          buyPhase board {_players=players', _buyList = buyList', _state=state''}
    else do
      putStrLn ("You were not able to buy anything else.")
      let players' = switchPlayer player (board ^. players)
      nextTurn board{_players=players'}

displayGameResults :: [Player] -> IO()
displayGameResults [] = putStrLn "Thanks for Playing Dominion with me"
displayGameResults players = do
  let (player, players') = splitFrom 0 0 players
  let player' = player!!0
  putStrLn (show (player' ^. T.playerName) ++ ": has ended with " ++ show (getPlayerScore (player')) ++ " points")
  displayGameResults players'


