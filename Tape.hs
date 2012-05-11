module Tape (
    Tape
  , newTape
  , left
  , right
  , incr
  , decr
  , inChar
  , outChar
  , is0
  ) where

import Control.Applicative


data Tape = Tape { _prev :: [Char], val :: Char, _next :: [Char] }

newTape :: Tape
newTape = Tape (repeat '\0') '\0' (repeat '\0')

left :: Tape -> Tape
left (Tape (l:ls) v rs) = Tape ls l (v:rs)

right :: Tape -> Tape
right (Tape ls v (r:rs)) = Tape (v:ls) r rs

incr :: Tape -> Tape
incr (Tape ls v rs) = Tape ls v' rs
  where v' = if v >= '\256' then '\0' else succ v

decr :: Tape -> Tape
decr (Tape ls v rs) = Tape ls v' rs
  where v' = if v <= '\0' then '\256' else pred v

inChar :: Tape -> IO Tape
inChar (Tape ls _ rs) = (\c -> Tape ls c rs) <$> getChar

outChar :: Tape -> IO ()
outChar (Tape _ c _) = putChar c

is0 :: Tape -> Bool
is0 (Tape _ '\0' _) = True
is0 _ = False
