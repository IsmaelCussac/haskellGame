{-# OPTIONS_HADDOCK ignore-exports #-}

module Morpion (Morpion) where

import Control.Exception
import Game
import Data.Map


data Symbole = X | O | Rien 
	deriving (Eq,Ord)

instance Show Symbole where
	show X = "X"
	show O = "O"
	show Rien = " "


autre :: Symbole -> Symbole
autre X = O
autre O = X

-- | Une position du jeu du Morpion est donnÃ©e par une trio :
-- le symbole a joué
-- le jouer qui doit jouer 
-- et les tas, c'est-Ã -dire, la grille des sybole un tableau de tableau, le 1er etant la grille entiere, et a l 'interieur les lignes 
data Morpion = Morpion Symbole Player [[Symbole]] deriving (Eq,Ord)

-- | La position initiale du jeu du Morpion
startPosition :: Morpion
startPosition =  Morpion X Adam [[Rien, Rien, Rien], [Rien, Rien, Rien], [Rien, Rien, Rien]]

-- show d'une ligne de la grille
showT :: [Symbole] -> String
showT (x:xs) 
	| xs == [] = show x 
	|otherwise = show x ++ " | " ++ showT xs

instance Show Morpion where
	show (Morpion sym pl (xs:ls)) 
				| ls == [] = showT xs ++ "\n"
				| otherwise = showT xs ++ "\n_________\n"++show (Morpion sym pl ls)

   
-- verifie qu'une case de la grille est bien vide 
checkVide :: [[Symbole]] -> (Int,Int) -> Bool
checkVide xs (i,j) 
	| (xs!!(i-1))!!(j-1) == Rien = True
	| otherwise = False


updates :: Symbole -> [[Symbole]] -> (Int,Int) ->  [[Symbole]]
updates symb xs (i,j) = 
	do 
		 
		take (i-1) xs 
		++ [ take (j-1) (xs !! (i-1)) 
		++ [symb]
		++ drop(j) (xs !! (i-1)) ] 
		++ drop(i) xs
--contruit une colonne
col :: Int -> Int -> [[Symbole]] -> [Symbole]
col i j heaps = ((heaps !! i) !! j) :((heaps !! (i+1)) !! j) : ((heaps !! (i+2)) !! j) : []

--construit une 1er diagonale
diagohb :: [[Symbole]] -> [Symbole]
diagohb heaps = ((heaps !! 0) !! 0) :((heaps !! 1) !! 1) : ((heaps !! 2) !! 2) : []

--construit la 2nd diagonale
diagobh :: [[Symbole]] -> [Symbole]
diagobh heaps = ((heaps !! 0) !! 2): ((heaps !! 1) !! 1) : ((heaps !! 2) !! 0) : []

-- voit si une position est gagnante (fin jeu)
win :: Morpion -> Bool
win (Morpion symbole thisplayer heaps) 
	|all (== (autre symbole)) (heaps !! 0) = True
	|all (== (autre symbole)) (heaps !! 1) = True
	|all (== (autre symbole)) (heaps !! 2) = True
	|all (== (autre symbole)) (col 0 0 heaps)= True
	|all (== (autre symbole)) (col 0 1 heaps)= True
	|all (== (autre symbole)) (col 0 2 heaps)= True
	|all (== (autre symbole)) (diagohb heaps)= True
	|all (== (autre symbole)) (diagobh heaps) = True
	| otherwise = False


-- | Cette fonction calcule les positions suivantes
-- dans le jeu du Morpion.
-- La rÃ¨gle est la suivante : si on se donne une position  de la forme Morpion symbole joueur [x1,.][..,xn],
-- alors une position suivante est de la forme Morpion autresymbole autrejoueur (update symbole [x1,.][..,xn] (i v)) 
-- oÃ¹ i est choisi parmi [1,...,n] et v peut prendre les valeurs dans l'intervalle [[1...n]]
choice :: Morpion -> [Morpion]
choice (Morpion symbole thisplayer heaps) = 
	[ Morpion symb nextplayer (updates symbole heaps (ligne,colonne))
	| ligne <- [1..nbHeap],
	colonne <- [1.. length (heaps !! 0)],
	checkVide heaps (ligne,colonne) == True
	]
	where
	nbHeap = length heaps
	nextplayer = flipPlayer thisplayer
	symb = autre symbole

-- cette fonction dit si il reste des choix ou non 
choices :: Morpion -> [Morpion]
choices (Morpion symbole thisplayer heaps)
	|(win (Morpion symbole (flipPlayer thisplayer) heaps)) == True = []
	|otherwise = choice (Morpion symbole thisplayer heaps) 

instance Game Morpion where
    initial = startPosition
    player (Morpion symb pl _) = pl  
    moves = choices 

