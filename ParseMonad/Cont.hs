module ParseMonad.Cont (
    ParseMonad
  , openBrace
  , closeBrace
  , runParseMonad
  ) where

import TapeP
import Program

import Control.Applicative
import Control.Monad.Cont

type ParseMonad = Cont ([TapeP] -> ([TapeP], TapeP))

openBrace :: ParseMonad TapeP -> ParseMonad TapeP
openBrace mcontinue = do
  continue <- mcontinue
  cont $ \k (break:bs) ->
    let loop    = loopControl continue break
        (ls, r) = k loop bs
    in (loop:ls, r)

closeBrace :: ParseMonad TapeP -> ParseMonad TapeP
closeBrace mbreak = do
  break <- mbreak
  cont $ \k bs ->
    let (loop:ls, r) = k loop (break:bs)
    in (ls, r)

runParseMonad :: ParseMonad TapeP -> Maybe TapeP
runParseMonad = Just . snd . ($ []) . (`runCont` (\a _ -> ([], a)))
