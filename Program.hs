module Program (
    Program (..)
  , branch
  , runProgram
  , step
  ) where

import Control.Monad (liftM)


data Program m = Instruction (m ()) (Program m)
               | Control (m (Program m))
               | Halt

branch :: Monad m => m Bool -> Program m -> Program m -> Program m
branch cond trueBranch falseBranch =
  Control ((\b -> if b then trueBranch else falseBranch) `liftM` cond)

runProgram :: Monad m => Program m -> m ()
runProgram = loopM step

loopM :: Monad m => (a -> m (Maybe a)) -> a -> m ()
loopM f = go
  where
    go a = do
      x <- f a
      case x of
        Nothing -> return ()
        Just a' -> go a'

step :: Monad m => Program m -> m (Maybe (Program m))
step (Instruction m p) = m >> return (Just p)
step (Control m) = Just `liftM` m
step Halt = return Nothing
