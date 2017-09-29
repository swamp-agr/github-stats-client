module Main where

import System.Environment

import Types
import CLI

main :: IO ()
main = do
    args <- getArgs
    case args of
      (config:command:[]) -> withConfig config command
      _                   -> showUsage