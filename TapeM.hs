{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

newtype TapeM a = TapeM { unTapeM :: StateT Tape IO a }
                deriving (Monad, Applicative, Functor)

left :: TapeM ()
left = TapeM $ modify T.left

right :: TapeM ()
right = TapeM $ modify T.right

incr :: TapeM ()
incr = TapeM $ modify T.incr

decr :: TapeM ()
decr = TapeM $ modify T.decr

inChar :: TapeM ()
inChar = TapeM $ get >>= liftIO . T.inChar >>= put

outChar :: TapeM ()
outChar = TapeM $ get >>= liftIO . T.outChar

is0 :: TapeM Bool
is0 = TapeM $ T.is0 <$> get

evalTapeM :: TapeM a -> Tape -> IO a
evalTapeM = evalStateT . unTapeM
