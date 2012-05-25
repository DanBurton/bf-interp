module TapeP (TapeP, loopControl) where

import TapeM
import Program

import Control.Applicative


type TapeP = Program TapeM

loopControl :: TapeP -> TapeP -> TapeP
loopControl = branch (not <$> is0)
