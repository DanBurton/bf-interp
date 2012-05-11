{-# LANGUAGE EmptyDataDecls, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Parser (toProgram) where

import TapeM
import Program

import Control.Applicative


type TapeP = Program TapeM

data MagicMonad :: * -> *
instance Monad MagicMonad
instance Applicative MagicMonad
instance Functor MagicMonad

runMagicMonad :: MagicMonad a -> a
runMagicMonad = undefined

toProgram :: String -> TapeP
toProgram = runMagicMonad . toProgramStep

data MagicToken :: *

pushBegin :: MagicMonad TapeP -> MagicMonad MagicToken
pushBegin = undefined

pushEnd   :: MagicMonad TapeP -> MagicToken -> MagicMonad TapeP
pushEnd = undefined

popBegin :: MagicMonad MagicToken
popBegin = undefined

popEnd   :: MagicToken -> MagicMonad TapeP
popEnd = undefined

liftI :: TapeM () -> String -> MagicMonad TapeP
liftI i cs = Instruction i <$> toProgramStep cs

-- this seems like it will be useful
loopControl :: TapeP -> TapeP -> TapeP
loopControl = branch is0

toProgramStep :: String -> MagicMonad TapeP
toProgramStep ('>':cs) = liftI right   cs
toProgramStep ('<':cs) = liftI left    cs
toProgramStep ('+':cs) = liftI incr    cs
toProgramStep ('-':cs) = liftI decr    cs
toProgramStep (',':cs) = liftI inChar  cs
toProgramStep ('.':cs) = liftI outChar cs
toProgramStep ('[':cs) = pushBegin (toProgramStep cs) >>= popEnd
toProgramStep (']':cs) = popBegin >>= pushEnd (toProgramStep cs)
toProgramStep (_:cs) = toProgramStep cs
toProgramStep [] = return Halt
