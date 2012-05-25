module Parser (toProgram) where

import TapeM
import TapeP
import Program
import ParseMonad.Cont (ParseMonad, openBrace, closeBrace, runParseMonad)

import Control.Applicative


toProgram :: String -> Maybe TapeP
toProgram = runParseMonad . toProgramStep

liftI :: TapeM () -> String -> ParseMonad TapeP
liftI i cs = Instruction i <$> toProgramStep cs

toProgramStep :: String -> ParseMonad TapeP
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
