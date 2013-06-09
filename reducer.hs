module Reducer (ISC(..), bf_reduce) where
import IR

-- Reduce Brainfuck instructions (from the IR) into more compact instructions
data ISC = Modify Int -- *ptr += x
         | Move Int -- ptr += x
         | OutI
         | LoopI [ISC]
         deriving (Show, Eq)

-- reduce Plus and Minus to an integer and a tail
reduceModifiers :: [Instruction] -> Int -> (Int, [Instruction])
reduceModifiers (Plus:xs) n = reduceModifiers xs (n+1)
reduceModifiers (Minus:xs) n = reduceModifiers xs (n-1)
reduceModifiers r n = (n, r)

reduceMovement (RightI:xs) n = reduceMovement xs (n+1)
reduceMovement (LeftI:xs) n = reduceMovement xs (n-1)
reduceMovement r n = (n, r)

bf_reduce ::  [Instruction] -> [ISC] -> [ISC]

-- base case
bf_reduce [] prg = reverse prg

-- beginning
bf_reduce (x:xs) [] =
	case x of
		Plus -> bf_reduce xs [Modify 1]
		Minus -> bf_reduce xs [Modify (-1)]
		LeftI -> bf_reduce xs [Move (-1)]
		RightI -> bf_reduce xs [Move 1]
		Out -> bf_reduce xs [OutI]
		Loop body -> bf_reduce xs [LoopI (bf_reduce body [])]

bf_reduce ins@(x:xs) prg@(p:ps) =
	case x of
		Plus -> reduceModify p $ reduceModifiers ins 0
		Minus -> reduceModify p $ reduceModifiers ins 0
		LeftI -> reduceMove p $ reduceMovement ins 0
		RightI -> reduceMove p $ reduceMovement ins 0
		Out -> bf_reduce xs (OutI:prg)
		Loop body -> bf_reduce xs (LoopI (bf_reduce body []) : prg)

	where
		reduceModify (Modify i) (n,r) = bf_reduce r (Modify (i+n) : ps) -- mutate existing Modify
		reduceModify _ (n,r) = bf_reduce r (Modify n : prg) -- add new Modify

		reduceMove (Move i) (n,r) = bf_reduce r (Move (i+n) : ps)
		reduceMove _ (n,r) = bf_reduce r (Move n : prg)