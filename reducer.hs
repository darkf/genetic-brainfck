module Reducer (ISC) where

-- Reduce Brainfuck instructions (from the IR) into more compact instructions
data ISC = Modify Int -- *ptr += x
         | Move Int -- ptr += x
         | Out
         | Loop [ISC]

