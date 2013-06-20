module Main (main) where
import Control.Exception (assert)
import IR (Instruction(..), bf_to_ir)
import Reducer (ISC(..), bf_reduce)
import Eval
import Data.Char (ord)
import System.Random
-- import Safe (at)
import Debug.Trace (trace)

(!!!) = (!!)

popSize = 64
geneLength = 64
targetString = "a"
charsetLength = 5
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

generateGenes :: StdGen -> String
generateGenes gen =
	map toChar $ take geneLength inf
	where
		inf = randomRs (0, charsetLength-1) gen
		toChar i = "+-<>." !!! i

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

crossover :: StdGen -> Individual -> Individual -> (Individual, StdGen)
crossover seed (Individual a) (Individual b) =
	(Individual $ reverse $ fst r, snd r)
	where r = foldr (\i (xs,seed) ->
		let (r, seed') = randomR (0.0, 1.0) seed in
		if r <= uniformRate then
			((a !!! i) : xs, seed')
		else
			((b !!! i) : xs, seed')
		) ([], seed) [0..geneLength-1]

mutate :: StdGen -> Individual -> (Individual, StdGen)
mutate seed (Individual a) =
	(Individual $ reverse $ fst r, snd r)
	where r = foldr (\i (xs,seed) ->
		let (r, seed') = randomR (0.0, 1.0) seed in
		if r <= mutationRate then
			let (r',seed'') = randomR (0, charsetLength-1) seed'
			in let c = "+-<>." !!! r'
			in
			(c : xs, seed'') -- add random gene
		else
			((a !!! i) : xs, seed')
		) ([], seed) [0..geneLength-1]

tournamentSelection :: StdGen -> Population -> (Individual, StdGen)
tournamentSelection seed (Population pop) =
	let r = foldr (\i (xs,seed) ->
			let (r, seed') = randomR (0, popSize-1) seed in
			((pop !!! r) : xs, seed')
		) ([], seed) [0..tournamentSize-1]
	    tournamentPop = Population $ reverse $ fst r
	    seed' = snd r
	    fittest = getFittest tournamentPop
	in
	(snd fittest, seed')

evolvePopulation :: StdGen -> Population -> (Population, StdGen)
evolvePopulation seed pop =
	let (_,keptBest) = getFittest pop
	    pop' = foldr (\i (xs,seed) ->
			let (a,seed') = tournamentSelection seed pop
			    (b,seed'') = tournamentSelection seed' pop
			    (c,seed''') = crossover seed'' a b
			    (c',seed'''') = mutate seed''' c
			in
			(c' : xs, seed'''')) ([], seed) [1..popSize-1]
	in
	-- keep best survivor from last generation
	let p = fst pop' ++ [keptBest] in
	(Population $ reverse $ p, snd pop')

generatePopulation :: IO [Individual]
generatePopulation =
	mapM (\_ -> do
			seed <- newStdGen
			return $ Individual $ generateGenes seed) [1..popSize]

main =
	do
	putStrLn $ "Target fitness of " ++ show targetString ++ " is: " ++ show targetFitness
	-- print $ eval_str targetStr
	-- print $ calcFitness (bf_to_ir program)
	-- print $ bf_to_ir program
	-- putStrLn "wat"
	-- seed <- newStdGen
	--let pop = map (\_ -> generateGenes seed) [1..popSize]
	pop <- generatePopulation
	-- print $ pop
	{- mapM_ (\ind@(Individual i) -> do
					putStrLn $ "individual: " ++ i
					print $ calcFitness ind
		) pop -}
	seed <- newStdGen
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