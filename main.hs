module Main (main) where
import Control.Monad.Random
import Control.Monad (forM)
import IR (bfToIR)
import Eval
import Data.Char (ord)

popSize = 64
geneLength = 128
targetString = "a"
charset = "+-<>."
randSeed = 42
charsetLength = length charset
targetFitness = fitnessOf targetString

uniformRate = 0.80 :: Float
mutationRate = 0.10 :: Float
tournamentSize = popSize `div` 3

type Individual = String
type Population = [Individual]

fitnessOf :: String -> Float
fitnessOf output =
	fromIntegral $ bfCmp output targetString
	where
		bfCmp [] _ = 0
		bfCmp _ [] = 0
		bfCmp (c:cs) (t:ts) =
			(256 - abs (ord t - ord c)) + bfCmp cs ts

		cmpStr :: String -> String -> Float
		cmpStr [] _ = 0
		cmpStr _ [] = 0
		cmpStr (c:cs) (t:ts) =
			if c == t then 1.0 + cmpStr cs ts
			else cmpStr cs ts

calcFitness :: Individual -> Float
calcFitness = fitnessOf . evalStr

evalStr :: String -> String
evalStr = bfEval . bfToIR

randomGenes :: Rand StdGen String
randomGenes = do
	inf <- getRandomRs (0, charsetLength-1)
	return $ map (charset !!) (take geneLength inf)

getFittest :: Population -> (Float, Individual)
getFittest pop =
	foldr maxFitness ((-1.0), head pop) pop
	where
		maxFitness :: Individual -> (Float, Individual) -> (Float, Individual)
		maxFitness x acc =
			let fitness = calcFitness x in
			if fitness > fst acc then
				(fitness, x)
			else acc

crossover :: Individual -> Individual -> Rand StdGen Individual
crossover a b = do
	indiv <- forM [0..geneLength-1] $ \i -> do
				r <- getRandomR (0.0, 1.0)
				if r <= uniformRate then
					return $ a !! i
				else
					return $ b !! i
	return indiv

mutate :: Individual -> Rand StdGen Individual
mutate a = do
	indiv <- forM [0..geneLength-1] $ \i -> do
		r <- getRandomR (0.0, 1.0)
		if r <= mutationRate then do
			r' <- getRandomR (0, charsetLength-1)
			return $ charset !! r' -- add random gene
		else
			return $ a !! i
	return indiv

tournamentSelection :: Population -> Rand StdGen Individual
tournamentSelection pop = do
	pop' <- forM [0..tournamentSize-1] $ \i -> do
		r <- getRandomR (0, popSize-1)
		return $ pop !! r
	let (_,fittest) = getFittest pop'
	return fittest

evolvePopulation :: Population -> Rand StdGen Population
evolvePopulation pop = do
	let (_,keptBest) = getFittest pop
	pop' <- forM [1..popSize-1] $ \i -> do
		a <- tournamentSelection pop
		b <- tournamentSelection pop
		c <- crossover a b
		mutate c
	-- keep best survivor from last generation
	return $ pop' ++ [keptBest]

generatePopulation :: Rand StdGen [Individual]
generatePopulation =
	forM [1..popSize] $ \_ -> randomGenes >>= return

main = do
	putStrLn $ "Target fitness of " ++ show targetString ++ " is: " ++ show targetFitness
	let (pop,seed) = runRand generatePopulation (mkStdGen randSeed)
	target <- loop 0 pop seed
	putStrLn $ "Reached target: " ++ show (snd target)
	putStrLn $ "Fitness: " ++ show (fst target)
	where
		loop :: Int -> Population -> StdGen -> IO (Float, Individual)
		loop gen pop seed =
			let (fitness,fittest) = getFittest pop in
			if fitness < targetFitness then
				do
					if gen `rem` 50 == 0 then
						putStrLn $ "Generation " ++ show gen ++ ", fitness: " ++ show fitness ++ ", fittest: " ++ show fittest ++ " (" ++ evalStr fittest ++ ")"
					else return ()
					let (evolved,seed') = runRand (evolvePopulation pop) seed
					loop (gen+1) evolved seed'
			else
				return (fitness, fittest)