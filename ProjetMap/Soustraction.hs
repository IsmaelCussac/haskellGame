{-# OPTIONS_HADDOCK ignore-exports #-}

module Soustraction (Soustraction) where

import Game

-- | Une position du jeu du Soustraction est donnÃ©e par une couple :
-- le jouer qui doit jouer 
-- le nombre d'étoiles
data Soustraction = Soustraction Player Int deriving (Eq,Ord)
instance Show Soustraction where
	show (Soustraction pl x) = show x

-- | La position initiale du jeu du Soustraction
startPosition :: Soustraction
startPosition =  Soustraction Adam 150

-- | Cette fonction met a jour la valeur d'étoiles restantes
update :: Int -> Int -> Int  
update x newValue = newValue

-- | Cette fonction calcule les positions suivantes
-- dans le jeu du Soustraction.
-- La rÃ¨gle est la suivante : si on se donne une position  de la forme Soustraction joueur x,
-- alors une position suivante est de la forme Soustraction autrejoueur (update x v) 
-- ou v peut prendre les valeurs  x-1, x-2, x-3
choices :: Soustraction -> [Soustraction]
choices (Soustraction thisplayer val) = 
	[ Soustraction nextplayer (update val newVal)
	| newVal <- [(val-3)..(val-1)], newVal >= 0
	]
	where
		nextplayer = flipPlayer thisplayer


instance Game Soustraction where
    initial = startPosition
    player (Soustraction pl _) = pl  
    moves = choices 

