module Parser (toProgram) where

import TapeM
import Program

import Control.Applicative
import Control.Monad.Cont


type TapeP = Program TapeM
type TapeC = Cont TapeP

toProgram :: String -> TapeP
toProgram = (`runCont` id) . toProgramStep


push :: TapeC TapeP -> TapeC TapeP
push mcontinue = do
  continue <- mcontinue
{-  cont (\breakMake -> let break = breakMake me
                          me = loopControl continue break
                      in me) -}
  cont (\breakMake -> loopControl continue (breakMake continue))

pop :: TapeC TapeP -> TapeC TapeP
pop mbreak = do
  break <- mbreak
{-  cont (\continueMake -> let continue = continueMake me
                             me = loopControl continue break
                         in me) -}
  cont (\continueMake -> loopControl (continueMake break) break)

liftI :: TapeM () -> String -> TapeC TapeP
liftI i cs = Instruction i <$> toProgramStep cs

loopControl :: TapeP -> TapeP -> TapeP
loopControl = branch is0

toProgramStep :: String -> TapeC TapeP
toProgramStep ('>':cs) = liftI right   cs
toProgramStep ('<':cs) = liftI left    cs
toProgramStep ('+':cs) = liftI incr    cs
toProgramStep ('-':cs) = liftI decr    cs
toProgramStep (',':cs) = liftI inChar  cs
toProgramStep ('.':cs) = liftI outChar cs
toProgramStep ('[':cs) = push (toProgramStep cs)
toProgramStep (']':cs) = pop (toProgramStep cs)
toProgramStep (_:cs) = toProgramStep cs
toProgramStep [] = return Halt
