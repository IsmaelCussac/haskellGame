{-# OPTIONS_HADDOCK ignore-exports #-}

module Soustraction (Soustraction) where

import Game

-- | Une position du jeu du Soustraction est donnÃ©e par une couple :
-- le jouer qui doit jouer 
-- le nombre d'étoiles
data Soustraction = Soustraction Player Int deriving (Eq,Ord,Show)

-- | La position initiale du jeu du Soustraction
startPosition :: Soustraction
startPosition =  Soustraction Adam 15

-- | Cette fonction met a jour la valeur d'étoiles restantes
update :: Int -> Int -> Int  
update x newValue =
    newValue

-- | Cette fonction calcule les positions suivantes
-- dans le jeu du Soustraction.
-- La rÃ¨gle est la suivante : si on se donne une position  de la forme Soustraction joueur x,
-- alors une position suivante est de la forme Soustraction autrejoueur (update x v) 
-- ou v peut prendre les valeurs dans l'intervalle [0,...,x]
choices :: Soustraction -> [Soustraction]
choices (Soustraction thisplayer val) = 
  [ Soustraction nextplayer (update val newVal)
  | newVal <- [1..3]
  ]
    where
      nextplayer = flipPlayer thisplayer

instance Game Soustraction where
    initial = startPosition
    player (Soustraction pl _) = pl  
    moves = choices 

