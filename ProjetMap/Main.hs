{-# OPTIONS_HADDOCK ignore-exports #-}

import Soustraction
import Nim
import Game
import Morpion
import Control.Exception

-- | la fonction main, qui sera appellÃ©e au debut du programme
main :: IO ()
main = 
	do
		putStr "Choisissez le jeu: 1=Nim / 2=Soustraction / 3=Morpion : " 
		m <- getLine
		let jeu :: Int
		    jeu = ((read m)::Int)
		result <- try (evaluate jeu) :: IO (Either SomeException Int)
		case result of
			Left e -> putStrLn (show e) >> main
			Right jeu -> do
				case jeu of
					1 -> do
				  			putStrLn "Nim"
				  			play (initial::Nim)
				  			return ()
				  	2 -> do
				  			putStrLn "Soustraction"
				  			play (initial::Soustraction)
					3 -> do
				  			putStrLn "Morpion"
				  			play (initial::Morpion)	
				  			
