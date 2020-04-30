{-# LANGUAGE TemplateHaskell, RecordWildCards, DuplicateRecordFields, NamedFieldPuns #-}
module Dominion.Effects where
import Dominion.Types as T
import Dominion.Cards
import Data.Maybe
import Data.List
import Control.Lens
import Lib.Array
import System.Random

gen = mkStdGen 123

shuffleDeck :: [Card] -> [Card]
shuffleDeck deck = shuffle' deck gen

discardCard :: Player -> IO Player
discardCard player = do
  let hand = player ^. T.hand
  if length hand > 0 then do
    showHand hand
    putStrLn "Which card do you want to discard?"
    response <- getLine
    let card = findCardByName response hand
    if isJust card then do
      let card' = fromJust card
      let hand' = removeCard card' hand
      discardCard player{_hand=hand', _discard=[card'] ++ (player ^. T.discard)}
    else return player
  else return player

discardHand :: Player -> Player
discardHand player@Player{..} = do
  player{_hand=[], _discard=_hand++_discard}

drawCards :: Player -> Int -> Player
drawCards player amm = do
  let deck' = player ^. T.deck
  if length deck' >= amm then do
    let (hand, deck'') = splitFrom 0 (amm-1) deck'
    player{_deck=deck'', _hand=player ^. T.hand ++ hand}
  else do
    let discard' = player ^. discard
    let (hand, deck'') = splitFrom 0 (amm - length deck' - 1) (shuffleDeck discard')
    player{_deck=deck'', _hand=player ^. T.hand ++ deck' ++ hand, _discard = []}

activateCardEffect :: Board -> CardEffect -> IO (Board)
activateCardEffect board (CoinValue x) = do
  let state = board ^. T.state
  let money = (state ^. T.money) + x
  return board{_state=state{_money=money}}
activateCardEffect board (GainAction x) = do
  let state = board ^. T.state
  let actions = (state ^. T.actions) + x
  return board{_state=state{_actions=actions}}
activateCardEffect board (GainBuy x) = do
  let state = board ^. T.state
  let buys = (state ^. T.buys) + x
  return board{_state=state{_buys=buys}}
activateCardEffect board (MerchantEffect) = do
  let state = board ^. T.state
  let merch = (state ^. T.merch) + 1
  putStrLn "The next Silver your play is worth 1 more"
  return board{_state=state{_merch=merch}}
activateCardEffect board (DrawCards x) = do
  let player = drawCards ((board ^. T.players)!!0) x
  putStrLn "Your new hand contains the following cards"
  showHand $ player ^. T.hand
  let players = replacePlayer player (board ^. T.players)
  return board{_players=players}
activateCardEffect board (CellarEffect) = do
  let player = (board ^. T.players)!!0
  let hand = player ^. T.hand
  player' <- discardCard player
  let player'' = drawCards player' (length hand - (length $ player' ^. T.hand))
  let players = replacePlayer player'' (board ^. T.players)
  putStrLn "Your new hand contains the following cards"
  showHand $ player'' ^. T.hand
  return board{_players=players}
activateCardEffect board (MineEffect) = do
  let player = (board ^. T.players)!!0
  let hand = player ^. T.hand
  let treasures = filter (isCardType T.Treasure) hand
  putStrLn "Which Treasure from your hand do you want to trash?"
  showHand treasures
  response <- getLine
  let card = findCardByName response treasures
  if isJust card then do
    let buyTreasure = filter (\x -> isCardType T.Treasure (T._card x)) (board ^. T.buyList)
    let buyTreasure' = canBuy buyTreasure $ (fromJust card ^. T.cost) + 3
    putStrLn ("You can gain: " ++ (intercalate ", " (map show buyTreasure')))
    gain <- getLine
    let card' = findCardByName gain (map (T._card) buyTreasure')
    if isJust card' then do
      let (list, gained) = buyCard gain $ board ^. T.buyList
      if isJust gained then do
        let hand' = (removeCard (fromJust card) hand) ++ [(fromJust gained)]
        let player' = player{_hand=hand'}
        let players = replacePlayer player' (board ^. T.players)
        showHand hand'
        return board{_players=players, _buyList=list}
      else return board
    else return board
  else return board
activateCardEffect board (RemodelEffect) = do
  let player = (board ^. T.players)!!0
  let hand = player ^. T.hand
  putStrLn "Which Card from your hand do you want to trash?"
  showHand hand
  response <- getLine
  let card = findCardByName response hand
  if isJust card then do
    let buyList' = canBuy (board ^. T.buyList) $ (fromJust card ^. T.cost) + 2
    putStrLn ("You can gain: " ++ (intercalate ", " (map show buyList')))
    gain <- getLine
    let card' = findCardByName gain (map (T._card) buyList')
    if isJust card' then do
      let (list, gained) = buyCard gain $ board ^. T.buyList
      if isJust gained then do
        let hand' = (removeCard (fromJust card) hand)
        let player' = player{_hand=hand', _discard=[fromJust gained] ++ (player ^. T.discard)}
        let players = replacePlayer player' (board ^. T.players)
        showHand hand'
        return board{_players=players, _buyList=list}
      else return board
    else return board
  else return board
activateCardEffect board _ = return board