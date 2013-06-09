module IR (Instruction(..), bf_to_ir) where

-- Simple IR for Brainfuck programs
data Instruction = LeftI
				 | RightI
				 | Plus
				 | Minus
				 | Out
				 | Loop [Instruction]
				 deriving (Show, Eq)

bf_to_ir :: String -> [Instruction]

bf_to_ir [] = []
bf_to_ir ('[':r) =
	let x = bf_to_ir r
	    y = take (length x - 1) x in
	case last x of
	  Loop r' -> Loop y : r'
	  _ -> error "unclosed loop?"
bf_to_ir (']':r) =
	[Loop (bf_to_ir r)]
bf_to_ir ('+':r) = Plus : bf_to_ir r
bf_to_ir ('-':r) = Minus : bf_to_ir r
bf_to_ir ('<':r) = LeftI : bf_to_ir r
bf_to_ir ('>':r) = RightI : bf_to_ir r
bf_to_ir ('.':r) = Out : bf_to_ir r

-----------------

-- >[+++]-+

