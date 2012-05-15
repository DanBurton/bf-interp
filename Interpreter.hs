module Interpreter (runBF) where

import TapeM (evalTapeM, newTape)
import Program (runProgram)
import Parser (toProgram)

runBF :: String -> IO ()
runBF str = case toProgram str of
  Just prog -> flip evalTapeM newTape $ runProgram prog
  Nothing   -> putStrLn "error runBF: ill-formed program"

helloWorld = runBF "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

main = runBF "++++++++++[>>++++++>+++++++++++>++++++++++>+++++++++>+++>+++++>++++>++++++++>+[<]<-]>>+++++++.>+.-.>+++.<++++.>>+++++++.<<++.+.>+++++.>.<<-.>---.<-----.-.+++++.>>>+++.-.<<-.<+..----.>>>>++++++++.>+++++++..<<<<+.>>>>-.<<<<.++++.------.<+++++.---.>>>>>.<<<++.<<---.>++++++.>>>>+.<<<-.--------.<<+.>>>>>>+++.---.<-.<<<<---.<.>---.>>>>>>."
