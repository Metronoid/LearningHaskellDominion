{-# LANGUAGE TemplateHaskell, RecordWildCards, DuplicateRecordFields, NamedFieldPuns #-}
module Dominion.Types where
import Control.Lens

data CardType = Treasure | Curse | Victory | Action deriving (Enum, Show, Eq, Ord)

data CardEffect = 
  EmptyEffect | 
  CoinValue Int | 
  VPValue Int | 
  GainAction Int | 
  GainBuy Int | 
  DrawCards Int | 
  MerchantEffect |
  CellarEffect deriving (Show, Eq, Ord)

data Card = Card {
  _cardName :: String, 
  _cardType :: CardType,
  _cost :: Int, 
  _effect :: [CardEffect]
}
makeLenses ''Card

instance Show Card where
  show Card{..} = _cardName++" ("++show _cost++")"

instance Eq Card where
  x == y = x ^. cardName == y ^. cardName

data Player = Player {
  _playerName :: String, 
  _deck :: [Card], 
  _discard :: [Card], 
  _hand :: [Card]
}
makeLenses ''Player

replacePlayer :: Player -> [Player] -> [Player]
replacePlayer y (x:xs) = [y] ++ xs

data State = State {
  _money :: Int,
  _actions :: Int, 
  _buys :: Int,
  _merch :: Int
} deriving (Show)
makeLenses ''State

baseState = State {_money = 0, _actions = 1, _buys = 1, _merch = 0}

data BuyableCard = BuyableCard {
  _card :: Card,
  _stock :: Int
}
makeLenses ''BuyableCard

data Board = Board {
  _players :: [Player],
  _state :: State,
  _buyList :: [BuyableCard]
}
makeLenses ''Board