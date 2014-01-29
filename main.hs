module Main (main) where
import Control.Monad.Random
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import IR (Instruction(..), bf_to_ir)
import Reducer (ISC(..), bf_reduce)
import Eval
import Data.Char (ord)

popSize = 64
geneLength = 64
targetString = "a"
charset = "+-<>."
randSeed = 42
charsetLength = length charset
targetFitness = fitnessOf targetString

uniformRate = 0.8 :: Float
mutationRate = 0.05 :: Float
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
			if c == t then
				1.0 + cmpStr cs ts
			else
				cmpStr cs ts

calcFitness :: Individual -> Float
calcFitness str = fitnessOf $ eval_str str

eval_str :: String -> String
eval_str str = bf_eval $ bf_reduce (bf_to_ir str) []

generateGenes :: Rand StdGen String
generateGenes = do
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
	return $ reverse indiv

mutate :: Individual -> Rand StdGen Individual
mutate a = do
	indiv <- forM [0..geneLength-1] $ \i -> do
		r <- getRandomR (0.0, 1.0)
		if r <= mutationRate then do
			r' <- getRandomR (0, charsetLength-1)
			return $ charset !! r' -- add random gene
		else
			return $ a !! i
	return $ reverse indiv

tournamentSelection :: Population -> Rand StdGen Individual
tournamentSelection pop = do
	pop' <- forM [0..tournamentSize-1] $ \i -> do
		r <- getRandomR (0, popSize-1)
		return $ pop !! r
	let (_,fittest) = getFittest $ reverse pop'
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
	let p = pop' ++ [keptBest]
	return $ reverse p

generatePopulation :: Rand StdGen [Individual]
generatePopulation =
	forM [1..popSize] $ \_ -> generateGenes >>= return

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
						liftIO $ putStrLn $ "Generation " ++ show gen ++ ", fitness: " ++ show fitness ++ ", fittest: " ++ show fittest ++ " (" ++ eval_str fittest ++ ")"
					else return ()
					let (evolved,seed') = runRand (evolvePopulation pop) seed
					loop (gen+1) evolved seed'
			else
				return (fitness, fittest)