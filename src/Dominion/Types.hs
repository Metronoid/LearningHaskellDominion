{-# LANGUAGE TemplateHaskell, RecordWildCards, DuplicateRecordFields, NamedFieldPuns #-}
module Dominion.Types where
import Control.Lens

data CardType = Treasure | Curse | Victory | Action deriving (Enum, Show, Eq, Ord)

data CardEffect = EmptyEffect | CoinValue Int | VPValue Int | GainAction Int | CellarEffect deriving (Show, Eq, Ord)

data Card = Card {
  _cardName :: String, 
  _cardType :: CardType,
  _cost :: Int, 
  _effect :: [CardEffect]
}
makeLenses ''Card

data Player = Player {
  _playerName :: String, 
  _deck :: [Card], 
  _discard :: [Card], 
  _hand :: [Card]
}
makeLenses ''Player

data State = State {
  _money :: Int,
  _actions :: Int, 
  _buys :: Int
} deriving (Show)
makeLenses ''State

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