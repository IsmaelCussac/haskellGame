{-# OPTIONS_HADDOCK ignore-exports #-}

module Morpion (Morpion) where

import Game

-- | Une position du jeu du Morpion est donnÃ©e par une couple :
-- le jouer qui doit jouer 
-- et les tas, c'est-Ã -dire, la liste d'entiers, chaque entier Ã©tant la longueur d'un tas 
data Morpion = Morpion Player [Int] deriving (Eq,Ord,Show)

-- | La position initiale du jeu du Morpion
startPosition :: Morpion
startPosition =  Morpion Adam [5,4,3,2,1]

-- | Cette fonction modifie une liste, en une position donnÃ©e, en remplacant un vieu valeur avec nouvau valeur
-- Par exemple, update [1,2,3] 2 4 donnera [1,4,3]
update :: 
    [a] -- ^ xs, la liste Ã  modifier  
    -> Int    -- ^ index, la position Ã  modifier, on compte Ã  partir de 1
    -> a      -- ^ newValue, la nouvelle valeur
    -> [a]   -- ^ la liste, misa Ã  jour
update xs index newValue =
    take (index-1) xs ++ [newValue] ++ drop index xs 

-- | Cette fonction calcule les positions suivantes
-- dans le jeu du Morpion.
-- La rÃ¨gle est la suivante : si on se donne une position  de la forme Morpion joueur [x1,...,xn],
-- alors une position suivante est de la forme Morpion autrejoueur (update [x1,...,xn] i v) 
-- oÃ¹ i est choisi parmi [1,...,n] et v peut prendre les valeurs dans l'intervalle [0,...,xi -1]
choices :: Morpion -> [Morpion]
choices (Morpion thisplayer heaps) = 
  [ Morpion nextplayer (update heaps indexHeap nbTokenLeft)
  | indexHeap <- [1..nbHeap],
    nbTokenLeft <- [0.. (heaps !! (indexHeap-1) - 1)]
  ]
    where
      nbHeap = length heaps
      nextplayer = flipPlayer thisplayer

instance Game Morpion where
    initial = startPosition
    player (Morpion pl _) = pl  
    moves = choices 

