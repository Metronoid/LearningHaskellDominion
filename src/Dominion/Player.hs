{-# LANGUAGE TemplateHaskell, RecordWildCards, DuplicateRecordFields, NamedFieldPuns #-}
module Dominion.Player where
import Control.Lens
import Dominion.Cards as Cards
import Dominion.Types as T 

blue = Player {_playerName = "Blue", _deck = starterDeck, _discard = [], _hand = []}
red = Player {_playerName = "Red", _deck = starterDeck, _discard = [], _hand = []}

getPlayerScore :: Player -> Int
getPlayerScore player = do
  let scores = deck' ++ discard'
  sum scores
  where
    deck' = map countValue (effects (player ^. deck))
    discard' = map countValue (effects (player ^. discard))
    effects cards = concatMap T._effect cards
    countValue (T.VPValue x) = x
    countValue _ = 0

starterDeck :: [Card]
starterDeck = [copper, copper, copper, copper, copper, copper, copper, estate, estate, estate]