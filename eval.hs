module Eval (bf_eval) where
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST (runST)
import Data.Char (chr)
import Reducer

cellSize = 3000

bf_eval :: [ISC] -> String
bf_eval instrs =
	runST $ bf_eval' instrs
	where
	bf_eval' instrs = do
		cells <- MV.replicate cellSize (0 :: Int)
		eval cells instrs 0 ""

		where
		eval cells [] _ out = return $ reverse out
		eval cells (x:xs) ptr out =
			case x of
				Modify i ->
					if ptr < 0 || ptr >= cellSize then
						return ""
					else do
						p <- MV.read cells ptr
						MV.write cells ptr (p + i)
						eval cells xs ptr out
				Move i -> eval cells xs (ptr + i) out
				OutI -> do
					if ptr < 0 || ptr >= cellSize then
						return ""
					else do
						c <- MV.read cells ptr
						if c < 0 || c > 127 then
							return ""
						else
							eval cells xs ptr (chr c:out)
				{-LoopI body ->
					let loop ptr' out' = do
						p <- MV.read cells ptr
						if p == 0 then
							return out'
						else do
							out'' <- eval cells body ptr' out'
							loop ptr' out''
					in do
						out' <- loop ptr out
						eval cells xs ptr out'-}