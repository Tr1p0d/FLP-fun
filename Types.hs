module Types where

import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Monad

type Symbol = Char
type Order = Int
type Diagram = (Char, Char)
type Trigram = (Char, Char, Char)
type Prob = Double

data Symbols = Symbols (M.Map Prob [Symbol]) (M.Map Symbol Prob) deriving Show
data Diagrams = Diagrams (M.Map Prob [Diagram]) (M.Map Diagram Prob)
data Trigrams = Trigrams (M.Map Prob [Trigram]) (M.Map Trigram Prob)

data LanStatistics = LanStatistics {
  getSymbols :: Symbols
, getDiagrams :: Diagrams
, getTrigrams :: Trigrams
}

stats = Symbols ( M.fromList . zip [1,2,3] $ [['a'],['b'],"caqwe"] ) (M.fromList [])

probToSym :: Prob -> Symbols -> [Symbol]
probToSym k (Symbols pts _) = fromJust . M.lookup k $ pts

symToProb :: Symbol -> Symbols -> Prob
symToProb  s (Symbols _ stp) = fromJust . M.lookup s $ stp

symToOrder :: Symbol -> Symbols -> Order
symToOrder s (Symbols pts stp) = fromJust $ flip M.lookup stp >=> flip M.lookupIndex pts $ s

orderToSym :: Order -> Symbols -> [Symbol]
orderToSym s (Symbols pts _) = snd . M.elemAt s $ pts





