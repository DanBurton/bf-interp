import TapeM
import Program

import Control.Applicative

replicateI :: Monad m => Int -> m () -> Program m -> Program m
replicateI 0 i p = p
replicateI n i p = Instruction i (replicateI (n - 1) i p)

-- translated from Wikipedia
-- http://en.wikipedia.org/wiki/Brainfuck#Hello_World.21

helloWorld :: Program TapeM
helloWorld = replicateI 10 incr -- initialize counter (cell #0) to 10
           $ loopKnot initStuff printStuff

loopControl :: Program TapeM -> Program TapeM -> Program TapeM
loopControl = branch (not <$> is0)

loopKnot :: (Program TapeM -> Program TapeM) -> Program TapeM -> Program TapeM
loopKnot continueMake break =
  let loop = loopControl (continueMake loop) break
  in loop

-- use loop to set the next four cells to 70/100/30/10
initStuff :: Program TapeM -> Program TapeM
initStuff loop =
    Instruction right $ replicateI 7 incr  -- add  7 to cell #1
  $ Instruction right $ replicateI 10 incr -- add 10 to cell #2
  $ Instruction right $ replicateI 3 incr  -- add  3 to cell #3
  $ Instruction right $ Instruction incr   -- add  1 to cell #4
  $ replicateI 4 left $ Instruction decr   -- decrement counter
  $ loop

printStuff :: Program TapeM
printStuff =
  -- print 'H'
    Instruction right
  $ replicateI 2 incr
  $ Instruction outChar
  -- print 'e'
  $ Instruction right
  $ Instruction incr
  $ Instruction outChar
  -- print 'l'
  $ replicateI 7 incr
  $ Instruction outChar
  -- print 'l'
  $ Instruction outChar
  -- print 'o'
  $ replicateI 3 incr
  $ Instruction outChar
  -- print ' '
  $ Instruction right
  $ replicateI 2 incr
  $ Instruction outChar
  -- print 'W'
  $ replicateI 2 left
  $ replicateI 15 incr
  $ Instruction outChar
  -- print 'o'
  $ Instruction right
  $ Instruction outChar
  -- print 'r'
  $ replicateI 3 incr
  $ Instruction outChar
  -- print 'l'
  $ replicateI 6 decr
  $ Instruction outChar
  -- print 'd'
  $ replicateI 8 decr
  $ Instruction outChar
  -- print '!'
  $ Instruction right
  $ Instruction incr
  $ Instruction outChar
  -- print '\n'
  $ Instruction right
  $ Instruction outChar
  $ Halt

main = evalTapeM (runProgram helloWorld) newTape
