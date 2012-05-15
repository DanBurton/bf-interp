import TapeM
import Program

import Control.Applicative

replicateI :: Monad m => Int -> m () -> Program m -> Program m
replicateI 0 i p = p
replicateI n i p = Instruction i (replicateI (n - 1) i p)

loopControl :: Program TapeM -> Program TapeM -> Program TapeM
loopControl = branch (not <$> is0)

loopKnot :: (Program TapeM -> Program TapeM) -> Program TapeM -> Program TapeM
loopKnot continueMake break =
  let loop = loopControl (continueMake loop) break
  in loop


replicateI' :: Int -> (Program TapeM -> Program TapeM) -> Program TapeM
            -> Program TapeM
replicateI' n f p =
    replicateI n incr
  $ loopKnot (\k -> Instruction right
                  $ f
                  $ Instruction left
                  $ Instruction decr
                  $ k)
             (Instruction right p)

hi :: Program TapeM
hi = replicateI' 104 (Instruction incr)
   $ Instruction outChar
   $ Instruction incr
   $ Instruction outChar
   $ Halt

main = evalTapeM (runProgram hi) newTape
