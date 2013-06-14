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

bf_expand :: [ISC] -> [Instruction] -> [Instruction]
bf_expand [] acc = reverse acc
bf_expand (x:xs) acc =
	case x of
		Modify i ->
			if i > 0 then bf_expand xs (replicate i Plus ++ acc)
			else bf_expand xs (replicate i Minus ++ acc)
		Move i ->
			if i > 0 then bf_expand xs (replicate i RightI ++ acc)
			else bf_expand xs (replicate i LeftI ++ acc)
		-- LoopI body -> bf_expand xs (Loop body ++ acc)
		OutI -> bf_expand xs (Out : acc)

bf_to_str :: [Instruction] -> String -> String
bf_to_str [] str = reverse str
bf_to_str (x:xs) str =
	case x of
		Plus -> bf_to_str xs ('+':str)
		Minus -> bf_to_str xs ('-':str)
		LeftI -> bf_to_str xs ('<':str)
		RightI -> bf_to_str xs ('>':str)
		Out -> bf_to_str xs ('.':str)

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

generatePopulation :: IO [Individual]
generatePopulation =
	mapM (\_ -> do
			seed <- newStdGen
			return $ Individual $ generateGenes seed) [1..popSize]

main =
	let
	    program = "[]>+-<..>>[]].-<+.<+<-..+>][+--+" -- random popSize-length string
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
	seed <- newStdGen
	--let pop = map (\_ -> generateGenes seed) [1..popSize]
	pop <- generatePopulation
	-- print $ pop
	mapM_ (\ind@(Individual i) -> do
					putStrLn $ "individual: " ++ i
					print $ calcFitness ind
		) pop