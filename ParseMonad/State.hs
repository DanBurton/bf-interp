{-# LANGUAGE DoRec #-}

module ParseMonad.State (
    ParseMonad
  , openBrace
  , closeBrace
  , runParseMonad
  ) where

import TapeP
import Program

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)

type ParseMonad = MaybeT (StateT [TapeP] (State [TapeP]))

push :: TapeP -> ParseMonad ()
push p = lift $ modify (p:)

pop :: ParseMonad TapeP
pop = do
  (x:xs) <- get
  put xs
  return x

-- two lifts will get us past the StateT [TapeP]
-- so it can work on the inner State [TapeP]

push' :: TapeP -> ParseMonad ()
push' p = lift $ lift $ modify (p:)

pop' :: ParseMonad TapeP
pop' = lift $ lift $ do
  (x:xs) <- get
  put xs
  return x

openBrace :: ParseMonad TapeP -> ParseMonad TapeP
openBrace mcontinue = do
  rec continue <- push loop >> mcontinue
      loop     <- do
            break <- pop'
            return $ loopControl continue break
  return loop

closeBrace :: ParseMonad TapeP -> ParseMonad TapeP
closeBrace mbreak = do
  loop  <- pop
  break <- mbreak
  push' break
  return loop

-- copied from Control.Monad.Trans.Maybe in transformers-0.3.0.0
instance (MonadFix m) => MonadFix (MaybeT m) where
  mfix f = MaybeT (mfix (runMaybeT . f . unJust))
    where unJust = fromMaybe (error "mfix MaybeT: Nothing")

runParseMonad :: ParseMonad TapeP -> Maybe TapeP
runParseMonad = flip evalState [] . flip evalStateT [] . runMaybeT

