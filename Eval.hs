module Eval (bfEval) where
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST (runST)
import Data.Char (chr)
import IR
import Reducer

cellSize = 3000

-- Evaluate Brainf*ck IR
bfEval instrs =
	runST $ bfEval' instrs
	where
	bfEval' instrs = do
		cells <- MV.replicate cellSize (0 :: Int)
		eval cells instrs 0 ""

		where
		eval cells [] _ out = return $ reverse out
		eval cells (x:xs) ptr out =
			case x of
				Plus -> modify 1
				Minus -> modify (-1)
				LeftI -> eval cells xs (clampPtr (ptr + 1)) out
				RightI -> eval cells xs (clampPtr (ptr - 1)) out
				Out -> do
						c <- MV.read cells clampedPtr
						if c < 0 || c > 127 then return ""
						else eval cells xs ptr (chr c:out)
			where
				clampPtr ptr =
					if ptr < 0 then cellSize + ptr
					else if ptr >= cellSize then cellSize - ptr
					else ptr
				clampedPtr = clampPtr ptr
				modify i = do
					p <- MV.read cells clampedPtr
					MV.write cells clampedPtr (p + i)
					eval cells xs clampedPtr out

-- Evaluate ISC instructions
bfEvalISC :: [ISC] -> String
bfEvalISC instrs =
	runST $ bfEval' instrs
	where
	bfEval' instrs = do
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