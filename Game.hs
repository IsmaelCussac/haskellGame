{-# OPTIONS_HADDOCK ignore-exports #-}

module Game (
  Player(Adam,Eve), 
  Game(initial,player,moves),
  flipPlayer,
  solve,
  play,
  Moves,
  Solution
  )
where

-- | Le type des donnÃ©es des joueur
data Player = Adam | Eve 
              deriving (Eq,Ord,Show)

-- | Le type des donnÃ©es des coups
type Moves a = [a]

-- | un jeu est identifiÃ© Ã  l'enesmble de ses positions (le type a)
-- avec les methodes de la classe Game 
class Game a where 
    -- | la position intiale du jeu a
    initial :: a
    -- | le joueur qui doit choisir un coup, dans une position donnÃ©e
    player :: a -> Player
    -- | les coups possibles, Ã  partir d'une position donnÃ©e
    moves :: a -> Moves a


-- | l'autre joueur 
flipPlayer :: Player -> Player
flipPlayer Adam = Eve
flipPlayer _ = Adam

-- | le type des solution.
-- Une solution est une fonction (partielle)
-- qui associe Ã  chaque position le joueur qui possÃ¨de une stratÃ©gie  gagnante
-- depuis cette position
-- ici, une telle fonction partielle est realisÃ©e comme une liste associative
type Solution a = [(a,Player)] 

		
-- | resoudre un jeu
solve :: (Game a, Ord a) => a -> Solution a -> Solution a
solve position precomputed =
  	case lookup position precomputed of
  	Just res -> precomputed 
  	Nothing -> 
		if (moves position == []) then
			(position, flipPlayer (player position)):precomputed
		else
      		let 
      		-- 	improvedPrecomputed = ??? solve (moves position)
       	 	    improvedPrecomputed = map f [(position, player position)]
       	 	    	where f = solve (moves position)
      		in 
      		-- 	if elem (player position) (map (??? improvedPrecomputed) (moves position)) then
				if elem (player position) (map (\x -> player x... improvedPrecomputed) (moves position)) then
		    		(position,player position):precomputed
				else
					(position,flipPlayer (player  position)):precomputed
	{-
solveNaive :: (Game a, Ord a) => a -> Player
solveNaive position =
	if (moves position == []) then
		flipPlayer (player  position)
	else
		if elem (player position) (map solveNaive (moves position)) then
		    player position
		else
			flipPlayer (player  position)
	
-}
-- | jouer un jeu
play :: (Game a,Ord a,Show a) => a -> IO ()
play position =
    let
        solution = solve position [(position, player position)]
    in
      	return ()

-- moves = choices = Nim -> [Nim]
-- Solution a = [(a, Player)]
-- Moves a = [a]
-- Game contient initial, player, moves

