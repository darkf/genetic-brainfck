module Reducer (ISC(..), bf_reduce) where
import IR

-- Reduce Brainfuck instructions (from the IR) into more compact instructions
data ISC = Modify Int -- *ptr += x
         | Move Int -- ptr += x
         | OutI
         | LoopI [ISC]
         deriving (Show, Eq)

-- Count and reduce instructions
reduceInstruction :: [Instruction] -> (Instruction, Instruction) -> Int -> (Int, [Instruction])
reduceInstruction [] _ n = (n, [])
reduceInstruction r@(x:xs) ins@(ins_inc, ins_dec) n
  | x == ins_inc = reduceInstruction xs ins (n+1)
  | x == ins_dec = reduceInstruction xs ins (n-1)
  | otherwise = (n, r)

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
		Plus -> reduceModify p $ reduceInstruction ins (Plus, Minus) 0
		Minus -> reduceModify p $ reduceInstruction ins (Plus, Minus) 0
		LeftI -> reduceMove p $ reduceInstruction ins (RightI, LeftI) 0
		RightI -> reduceMove p $ reduceInstruction ins (RightI, LeftI) 0
		Out -> bf_reduce xs (OutI:prg)
		Loop body -> bf_reduce xs (LoopI (bf_reduce body []) : prg)

	where
		reduceModify (Modify i) (n,r) = bf_reduce r (Modify (i+n) : ps) -- mutate existing Modify
		reduceModify _ (n,r) = bf_reduce r (Modify n : prg) -- add new Modify

		reduceMove (Move i) (n,r) = bf_reduce r (Move (i+n) : ps)
		reduceMove _ (n,r) = bf_reduce r (Move n : prg)