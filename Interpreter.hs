module Interpreter (runBF) where

import TapeM (evalTapeM, newTape)
import Program (runProgram)
import Parser (toProgram)

runBF :: String -> IO ()
runBF = flip evalTapeM newTape . runProgram . toProgram
