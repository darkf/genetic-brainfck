module Main (main) where
import Control.Exception (assert)
import IR (Instruction(..), bf_to_ir)
import Reducer (ISC(..), bf_reduce)
import Eval
import Data.Char (ord)
import System.Random

popSize = 32
geneLength = 32
charsetLength = 5
targetFitness = 2.0 :: Float

uniformRate = 0.5 :: Float
mutationRate = 0.015 :: Float
tournamentSize = 5

newtype Individual = Individual String
newtype Population = Population [Individual]

instance Show Individual where
	show (Individual a) = show a

calcFitness :: Individual -> Float
calcFitness (Individual str) =
	fitnessOf $ bf_eval $ bf_reduce ir []
	where
		ir = bf_to_ir str
		fitnessOf :: String -> Float
		fitnessOf output =
			cmpStr output "hi"
		cmpStr :: String -> String -> Float
		cmpStr [] _ = 0
		cmpStr _ [] = 0
		cmpStr (c:cs) (t:ts) =
			if c == t then
				1.0 + cmpStr cs ts
			else
				cmpStr cs ts

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
		toChar i = "+-<>." !! i

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

-- from StackOverflow, because I'm lazy
replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

crossover :: StdGen -> Individual -> Individual -> Individual
crossover seed (Individual a) (Individual b) =
	Individual $ reverse.fst $ foldr (\i (xs,seed) ->
		let (r, seed') = randomR (0.0, 1.0) seed in
		if r <= uniformRate then
			((a !! i) : xs, seed')
		else
			((b !! i) : xs, seed')
		) ([], seed) [0..geneLength]

generatePopulation :: IO [Individual]
generatePopulation =
	mapM (\_ -> do
			seed <- newStdGen
			return $ Individual $ generateGenes seed) [1..popSize]

main =
	let
	    -- program = "[]>+-<..>>[]].-<+.<+<-..+>][+--+" -- random popSize-length string
	    -- targetISC = [Modify (ord 'h'), OutI, Move 1, Modify (ord 'i'), OutI]
	    -- targetIR = bf_expand targetISC []
	    targetStr = "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++."
	    --program = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
	    --ir = bf_to_ir program
	    --reduced = bf_reduce ir []
	in
	do
	-- print $ eval_str targetStr
	-- print $ calcFitness (bf_to_ir program)
	-- print $ bf_to_ir program
	-- putStrLn "wat"
	-- seed <- newStdGen
	--let pop = map (\_ -> generateGenes seed) [1..popSize]
	pop <- generatePopulation
	-- print $ pop
	mapM_ (\ind@(Individual i) -> do
					putStrLn $ "individual: " ++ i
					print $ calcFitness ind
		) pop