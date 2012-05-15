{-# LANGUAGE DoRec, TypeSynonymInstances, FlexibleInstances #-}

module Parser (toProgram) where

import TapeM
import Program

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)

type TapeP = Program TapeM
type ParseState = MaybeT (StateT [TapeP] (State [TapeP]))

push :: TapeP -> ParseState ()
push p = lift $ modify (p:)

pop :: ParseState TapeP
pop = do
  (x:xs) <- get
  put xs
  return x

-- two lifts will get us past the StateT [TapeP]
-- so it can work on the inner State [TapeP]

push' :: TapeP -> ParseState ()
push' p = lift $ lift $ modify (p:)

pop' :: ParseState TapeP
pop' = lift $ lift $ do
  (x:xs) <- get
  put xs
  return x

-- copied from Control.Monad.Trans.Maybe in transformers-0.3.0.0
instance (MonadFix m) => MonadFix (MaybeT m) where
  mfix f = MaybeT (mfix (runMaybeT . f . unJust))
    where unJust = fromMaybe (error "mfix MaybeT: Nothing")


toProgram :: String -> Maybe TapeP
toProgram = flip evalState [] . flip evalStateT [] . runMaybeT . toProgramStep


openBrace :: ParseState TapeP -> ParseState TapeP
openBrace mcontinue = do
  rec continue <- push loop >> mcontinue
      loop     <- do
            break <- pop'
            return $ loopControl continue break
  return loop

closeBrace :: ParseState TapeP -> ParseState TapeP
closeBrace mbreak = do
  loop  <- pop
  break <- mbreak
  push' break
  return loop


liftI :: TapeM () -> String -> ParseState TapeP
liftI i cs = Instruction i <$> toProgramStep cs

loopControl :: TapeP -> TapeP -> TapeP
loopControl = branch is0

toProgramStep :: String -> ParseState TapeP
toProgramStep ('>':cs) = liftI right   cs
toProgramStep ('<':cs) = liftI left    cs
toProgramStep ('+':cs) = liftI incr    cs
toProgramStep ('-':cs) = liftI decr    cs
toProgramStep (',':cs) = liftI inChar  cs
toProgramStep ('.':cs) = liftI outChar cs
toProgramStep ('[':cs) = openBrace  (toProgramStep cs)
toProgramStep (']':cs) = closeBrace (toProgramStep cs)
toProgramStep (_:cs)   = toProgramStep cs
toProgramStep []       = return Halt
