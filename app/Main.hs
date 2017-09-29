module Main where

import System.Environment

import Types
import CLI

main :: IO ()
main = do
    args <- getArgs
    case args of
      (command:config:[]) -> withConfig config command
      _                   -> showUsage