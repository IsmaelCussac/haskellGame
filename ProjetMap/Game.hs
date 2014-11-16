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
import qualified Data.Map as Map
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
type Solution a = Map.Map Player a

				
--showplayer :: Solution a -> String
--showplayer (pos,pla) = show pla

-- affiche toute les positions possible
showGrid ::(Show a) => [a] -> Int -> String
showGrid [] i = "\n"
showGrid (pos:xs) i = "Choix " ++ show i ++ " :\n " ++ show pos ++ "\n" ++ (showGrid xs (i+1))

--union des Map
solveAll m = Map.unions (map solve m)

-- dit si a partir d'une position, elle est gagnante pour qui ?
solve :: (Game a,Ord a) => a -> Solution a
solve position 
	| player position == Eve && 
		Map.member Eve solutions = Map.singleton Eve position
	| player position == Adam &&
		Map.notMember Adam solutions = Map.singleton Eve position
	| otherwise = Map.singleton Adam position
	where
		positions = moves position
		solutions = solveAll positions 
		pl = player position				

--Adam joue
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


--eve joue
evePlay :: (Game a, Eq a,Ord a, Show a) => a -> [a] -> IO (a)
evePlay position positions = 
		return (Map.findWithDefault (head positions) Eve solutions)
		where		
			solutions = solveAll positions


-- | jouer un jeu
play :: (Game a,Eq a,Ord a,Show a) => a -> IO ()
play position 
	| length(moves position) == 0 = do
		putStrLn (show position)
		putStrLn ("le gagnant est " ++ show ( flipPlayer (player position)) )
	| player position == Eve = do
		putStrLn ("\nC'est au tour de "++ show(player position))
		putStrLn "\nPosition actuelle"
		putStrLn(show position)
		position <- evePlay position positions
		play position
	|otherwise = do
		putStrLn ("\nC'est au tour de "++ show(player position))		
		putStrLn "\nPosition actuelle"
		putStrLn (show position)
		position <-adamPlay position positions
		play position
	where
		positions = moves position 



		
