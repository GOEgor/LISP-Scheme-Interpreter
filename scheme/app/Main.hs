module Main where

import Text.Megaparsec
import Parser
import Interpreter

main :: IO ()
main = repl defaultEnv
