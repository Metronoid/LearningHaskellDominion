{-# LANGUAGE TemplateHaskell, RecordWildCards, DuplicateRecordFields, NamedFieldPuns #-}
module Dominion.Cards where

import Control.Lens
import Dominion.Types

import Data.List

baseCard = Card {_cardName = "Empty", _cardType=Treasure, _cost = 0, _effect = [EmptyEffect]}
copper = Card {_cardName = "Copper", _cardType=Treasure, _cost = 0, _effect = [CoinValue 1]}
silver = Card {_cardName = "Silver", _cardType=Treasure, _cost = 3, _effect = [CoinValue 2]}
gold = Card {_cardName = "Gold", _cardType=Treasure, _cost = 6, _effect = [CoinValue 3]}
curse = Card {_cardName = "Curse", _cardType=Curse, _cost = 0, _effect = [VPValue (-1)]}
estate = Card {_cardName = "Estate", _cardType=Victory, _cost = 2, _effect = [VPValue 1]}
duchy = Card {_cardName = "Duchy", _cardType=Victory, _cost = 5, _effect = [VPValue 3]}
province = Card {_cardName = "Province", _cardType=Victory, _cost = 8, _effect = [VPValue 6]}

cellar = Card {_cardName="Cellar", _cardType=Action, _cost = 2, _effect=[GainAction 1, CellarEffect]}

instance Show Card where
  show Card{..} = _cardName++" ("++show _cost++")"

instance Eq Card where
  x == y = x ^. cardName == y ^. cardName

findCardsByName :: String -> [Card] -> [Card]
findCardsByName name' cards = filter (\x -> _cardName x == name') cards

findCardByName :: String -> [Card] -> Maybe Card
findCardByName name' cards = find (\x -> _cardName x == name') cards

instance Show BuyableCard where
  show BuyableCard{..} = show _card ++ ": " ++ show _stock

findBuyableCardsByName :: String -> [BuyableCard] -> [BuyableCard]
findBuyableCardsByName name' cards = filter (\x -> _cardName (x ^. card) == name') cards

listBuy :: Int -> BuyableCard -> [BuyableCard]
listBuy money buyable = do
  if money >= (buyable ^. card ^. cost) && buyable ^. stock > 0 then [buyable] else []
  -- where res = (name ++ " (" ++ (show cost) ++ ")")

buyCard :: String -> [BuyableCard] -> Card
buyCard name cards = do 
  let filterCards = findBuyableCardsByName name cards
  if length filterCards > 0 then do
    let buyable = filterCards!!0
    if buyable ^. stock > 0 then
      buyable ^. card
    else baseCard
  else baseCard

isAction :: Card -> Bool
isAction card = (card ^. cardType) == Action

lowerStock :: BuyableCard -> BuyableCard
lowerStock card@BuyableCard{_stock} = card {_stock=_stock-1} 

removeItem :: Card -> [Card] -> [Card]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys