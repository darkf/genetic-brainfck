module Main (main) where
import Control.Exception (assert)
import IR
import Reducer
import Eval

main =
	let program = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
	    ir = bf_to_ir program
	    reduced = bf_reduce ir []
	in
	-- print $ bf_to_ir program
	putStrLn program >>
	putStrLn "" >>
	print ir >>
	putStrLn "" >>
	print reduced >>
	putStrLn "" >>
	print (bf_eval reduced)