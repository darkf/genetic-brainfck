module IR (Instruction(..), bfToIR) where

-- Simple IR for Brainfuck programs
data Instruction = LeftI
				 | RightI
				 | Plus
				 | Minus
				 | Out
				 | Loop [Instruction]
				 deriving (Show, Eq)

bfToIR :: String -> [Instruction]

bfToIR [] = []
bfToIR ('[':r) =
	let x = bfToIR r
	    y = take (length x - 1) x in
	case last x of
	  Loop r' -> Loop y : r'
	  _ -> error "unclosed loop?"
bfToIR (']':r) = [Loop (bfToIR r)]
bfToIR ('+':r) = Plus : bfToIR r
bfToIR ('-':r) = Minus : bfToIR r
bfToIR ('<':r) = LeftI : bfToIR r
bfToIR ('>':r) = RightI : bfToIR r
bfToIR ('.':r) = Out : bfToIR r
