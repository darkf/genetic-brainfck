module Eval (bf_eval) where
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST (runST)
import Data.Char (chr)
import Reducer

bf_eval :: [ISC] -> String
bf_eval instrs =
	runST $ bf_eval' instrs
	where
	bf_eval' instrs = do
		cells <- MV.replicate 3000 (0 :: Int)
		eval cells instrs 0 ""

		where
		eval cells [] _ out = return $ reverse out
		eval cells (x:xs) ptr out =
			case x of
				Modify i -> do
					p <- MV.read cells ptr
					MV.write cells ptr (p + i)
					eval cells xs ptr out
				Move i -> eval cells xs (ptr + i) out
				OutI -> do
					c <- MV.read cells ptr
					eval cells xs ptr (chr c:out)
				LoopI body ->
					let loop ptr' out' = do
						p <- MV.read cells ptr
						if p == 0 then
							return out'
						else do
							out'' <- eval cells body ptr' out'
							loop ptr' out''
					in do
						out' <- loop ptr out
						eval cells xs ptr out'