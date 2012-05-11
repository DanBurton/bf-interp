module TapeM (
    TapeM
  , Tape
  , newTape
  , evalTapeM
  , left
  , right
  , incr
  , decr
  , inChar
  , outChar
  , is0
  ) where

import Tape (Tape, newTape)
import qualified Tape as T

import Control.Monad.State
import Control.Monad.IO.Class
import Control.Applicative

type TapeM = StateT Tape IO

left :: TapeM ()
left = modify T.left

right :: TapeM ()
right = modify T.right

incr :: TapeM ()
incr = modify T.incr

decr :: TapeM ()
decr = modify T.decr

inChar :: TapeM ()
inChar = get >>= liftIO . T.inChar >>= put

outChar :: TapeM ()
outChar = get >>= liftIO . T.outChar

is0 :: TapeM Bool
is0 = T.is0 <$> get

evalTapeM :: TapeM a -> Tape -> IO a
evalTapeM = evalStateT
