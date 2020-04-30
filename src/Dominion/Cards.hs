{-# LANGUAGE TemplateHaskell, RecordWildCards, DuplicateRecordFields, NamedFieldPuns #-}
module Dominion.Cards where

import Lib.Array
import Control.Lens
import Dominion.Types as T

import Data.List
import Data.Maybe

copper = Card {_cardName = "Copper", _cardType=Treasure, _cost = 0, _effect = [CoinValue 1]}
silver = Card {_cardName = "Silver", _cardType=Treasure, _cost = 3, _effect = [CoinValue 2]}
gold = Card {_cardName = "Gold", _cardType=Treasure, _cost = 6, _effect = [CoinValue 3]}
curse = Card {_cardName = "Curse", _cardType=Curse, _cost = 0, _effect = [VPValue (-1)]}
estate = Card {_cardName = "Estate", _cardType=Victory, _cost = 2, _effect = [VPValue 1]}
duchy = Card {_cardName = "Duchy", _cardType=Victory, _cost = 5, _effect = [VPValue 3]}
province = Card {_cardName = "Province", _cardType=Victory, _cost = 8, _effect = [VPValue 6]}

cellar = Card {_cardName="Cellar", _cardType=Action, _cost = 2, _effect=[CellarEffect]}
market = Card {_cardName="Market", _cardType=Action, _cost = 5, _effect=[DrawCards 1, GainBuy 1, CoinValue 1]}
smithy = Card {_cardName="Smithy", _cardType=Action, _cost = 4, _effect=[DrawCards 3, GainAction (-1)]}
village = Card {_cardName="Village", _cardType=Action, _cost = 3, _effect=[DrawCards 1, GainAction 1]}
merchant = Card {_cardName="Merchant", _cardType=Action, _cost = 3, _effect=[DrawCards 1, MerchantEffect]}
mine = Card {_cardName="Mine", _cardType=Action, _cost = 5, _effect=[MineEffect, GainAction (-1)]}
remodel = Card {_cardName="Remodel", _cardType=Action, _cost = 4, _effect=[RemodelEffect, GainAction (-1)]}

findCardsByName :: String -> [Card] -> [Card]
findCardsByName name' cards = filter (\x -> _cardName x == name') cards

findCardByName :: String -> [Card] -> Maybe Card
findCardByName name' cards = find (\x -> _cardName x == name') cards

instance Show BuyableCard where
  show BuyableCard{..} = show _card ++ ": " ++ show _stock

findBuyableCardsByName :: String -> [BuyableCard] -> [BuyableCard]
findBuyableCardsByName name' cards = filter (\x -> _cardName (x ^. card) == name') cards

  -- where res = (name ++ " (" ++ (show cost) ++ ")")

buyCard :: String -> [BuyableCard] -> ([BuyableCard], Maybe Card)
buyCard name cards = do 
  let filterCards = findBuyableCardsByName name cards
  if length filterCards > 0 then do
    let buyable = filterCards!!0
    if buyable ^. stock > 0 then do
      let elemCard = elemIndex (buyable ^. T.card) (map (T._card) cards)
      if isNothing elemCard then
        (cards, Nothing)
      else do
        let buyList' = replaceNth (fromJust elemCard) (lowerStock(cards!!(fromJust elemCard))) cards
        (buyList', Just $ buyable ^. card)
    else
      (cards, Nothing)
  else
    (cards, Nothing)

isCardType :: CardType -> Card -> Bool
isCardType cardType card = (card ^. T.cardType) == cardType

lowerStock :: BuyableCard -> BuyableCard
lowerStock card@BuyableCard{_stock} = card {_stock=_stock-1} 

removeCard :: Card -> [Card] -> [Card]
removeCard _ [] = []
removeCard x (y:ys) | x == y = ys
  | otherwise = y : removeCard x ys

showHand :: [Card] -> IO ()
showHand hand = do
  let cards = map (\x -> T._cardName x) hand
  putStrLn ("Hand: " ++ (intercalate " " cards))