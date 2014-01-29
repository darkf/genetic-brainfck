module Main (main) where
import Control.Exception (assert)
import Control.Monad.Random (Rand, runRand, evalRand, getRandomR, getRandomRs)
import Control.Monad (forM)
import IR (Instruction(..), bf_to_ir)
import Reducer (ISC(..), bf_reduce)
import Eval
import Data.Char (ord)
import System.Random

(!!!) = (!!)

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

newtype Individual = Individual String
newtype Population = Population [Individual]

instance Show Individual where
	show (Individual a) = show a

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
calcFitness (Individual str) =
	fitnessOf $ eval_str str

eval_str :: String -> String
eval_str str =
	bf_eval isc
	where
		ir = bf_to_ir str
		isc = bf_reduce ir []

generateGenes :: Rand StdGen String
generateGenes = do
	inf <- getRandomRs (0, charsetLength-1)
	return $ map toChar $ take geneLength inf
	where
		toChar i = charset !!! i

getFittest :: Population -> (Float, Individual)
getFittest (Population pop) =
	foldr maxFitness ((-1.0), head pop) pop
	where
		maxFitness :: Individual -> (Float, Individual) -> (Float, Individual)
		maxFitness x acc =
			let fitness = calcFitness x in
			if fitness > fst acc then
				(fitness, x)
			else acc

crossover :: Individual -> Individual -> Rand StdGen Individual
crossover (Individual a) (Individual b) = do
	indiv <- forM [0..geneLength-1] $ \i -> do
				r <- getRandomR (0.0, 1.0)
				if r <= uniformRate then
					return $ a !!! i
				else
					return $ b !!! i
	return $ Individual $ reverse indiv

mutate :: StdGen -> Individual -> (Individual, StdGen)
mutate seed (Individual a) =
	(Individual $ reverse $ fst r, snd r)
	where r = foldr (\i (xs,seed) ->
		let (r, seed') = randomR (0.0, 1.0) seed in
		if r <= mutationRate then
			let (r',seed'') = randomR (0, charsetLength-1) seed'
			in let c = charset !!! r'
			in
			(c : xs, seed'') -- add random gene
		else
			((a !!! i) : xs, seed')
		) ([], seed) [0..geneLength-1]

tournamentSelection :: Population -> Rand StdGen Individual
tournamentSelection (Population pop) = do
	pop' <- forM [0..tournamentSize-1] $ \i -> do
		r <- getRandomR (0, popSize-1)
		return $ pop !!! r
	let (_,fittest) = getFittest $ Population (reverse pop')
	return fittest

evolvePopulation :: Population -> Rand StdGen Population
evolvePopulation pop = do
	let (_,keptBest) = getFittest pop
	pop' <- forM [1..popSize-1] $ \i -> do
		a <- tournamentSelection pop
		b <- tournamentSelection pop
		c <- crossover a b
		return $ mutate c
	-- keep best survivor from last generation
	let p = fst pop' ++ [keptBest]
	return $ Population $ reverse p

generatePopulation :: Rand StdGen [Individual]
generatePopulation =
	mapM (\_ -> do
			generateGenes >>= return . Individual) [1..popSize]

main = do
	putStrLn $ "Target fitness of " ++ show targetString ++ " is: " ++ show targetFitness
	pop <- generatePopulation
	let seed = mkStdGen randSeed
	target <- loop 0 (Population pop) seed
	putStrLn $ "Reached target: " ++ show (snd target)
	putStrLn $ "Fitness: " ++ show (fst target)
	where
		fromInd (Individual i) = i
		loop :: Int -> Population -> StdGen -> IO (Float, Individual)
		loop gen pop seed =
			let (fitness,fittest) = getFittest pop in
			if fitness < targetFitness then
				do
					if gen `mod` 50 == 0 then
						putStrLn $ "Generation " ++ show gen ++ ", fitness: " ++ show fitness ++ ", fittest: " ++ show fittest ++ " (" ++ (eval_str (fromInd fittest)) ++ ")"
					else return ()
					let (evolved,seed') = evolvePopulation seed pop
					loop (gen+1) evolved seed'
			else
				return (fitness, fittest)