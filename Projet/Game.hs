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
import Control.Exception
-- | Le type des donnÃ©es des joueur
data Player = Adam | Eve 
	deriving (Eq,Ord,Show)

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
type Solution a = (a,Player)

joueur :: Solution a -> Player
joueur (pos, play) = play
				
showplayer :: Solution a -> String
showplayer (pos,pla) = show pla

-- affiche toute les positions possible
showGrid ::(Show a) => [a] -> Int -> String
showGrid [] i = "\n"
showGrid (pos:xs) i = "Choix " ++ show i ++ " :\n" ++ show pos ++ "\n\n" ++ (showGrid xs (i+1))

-- construit un tableau d'indices, correspondant aux indices des solutions gagnantes ( pour le moves)
prendreIndex ::(Eq b) => [a] -> ( a -> b) -> b -> [Int]
prendreIndex solutions joueur player =  [i | (solution,i)<- zip solutions [0..n] ,joueur solution ==player]
									where n = length solutions -1 

-- dit si a partir d'une position, elle est gagnante pour qui ?
solve :: (Game a,Eq a) => a -> Solution a
solve position
	|	heaps == [] = (position, flipPlayer (player position))  
	|	all (== flipPlayer (player position)) playerss = (position, flipPlayer (player position))
	|	otherwise = (position,player position)
	where
		heaps =  moves position
		positions = map solve heaps 
		playerss = map snd positions
		
-- a partir d'un tableau de positions, associe a chacune d'elle qui est gagnant 
solveAll :: (Game a,Eq a) =>[a] -> [Solution a]
solveAll positions = map solve positions

-- donne le premier indice d'une position gagnante
prendrePremIndexGagne :: (Game a, Eq a) => Player -> [Solution a]-> IO(Int) 
prendrePremIndexGagne playe solutions = return ((prendreIndex solutions joueur playe)!! 0)				

--c 'est a Eve de jouer
evePlay :: (Game a, Eq a, Show a) => a -> [a] -> IO (a)
evePlay position positions 	
	-- si la position actuelle est gagnante pour ADAM alors on prend au hazard(ici le 1er) dans positions			
	|joueur (solve position ) == Adam = return ((positions)!!0)
	| otherwise = do
		pos <- prendrePremIndexGagne Eve (solveAll positions)
		return ((positions)!!pos)

--c est a Adam de jouer
adamPlay :: (Game a, Eq a, Show a) => a -> [a] -> IO (a)
adamPlay position positions = 
				do
					putStrLn (showGrid positions 1)
					putStrLn "Rentrez votre choix : " 
					m <- getLine
					let jeu = ((read m)::Int)
					result <- try (evaluate jeu) :: IO (Either SomeException Int)
					case result of
						Left e -> putStrLn (show e) >> return position
						Right jeu -> do
							if ( jeu > (length positions)) then
								return position
							else if (jeu < 0) then 
								return position
							else 
								return ((positions) !! (jeu-1))


-- | jouer un jeu
play :: (Game a,Ord a,Show a) => a -> IO ()
play position
	| length(moves position) == 0 =
		do
			putStrLn (show position)
			putStrLn ("le gagnant est " ++ show ( flipPlayer (player position)) )
	| otherwise =
		do
			putStrLn ("\nC'est au tour de "++ show(player position))
	
			if (player position == Eve) then
				do 
					putStrLn "\nPosition actuelle"
					putStrLn (show position)
					position <- evePlay position (moves position)
					play position
		
			else
				do
					putStrLn "\nPosition actuelle"
					putStrLn (show position)
					position <- adamPlay position (moves position)
					play position

