{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Exception (throw)
import Data.Yaml
import Data.ByteString (readFile)
import Prelude hiding (readFile)
import System.Environment
import GHC.Generics

main :: IO ()
main = do
    args <- getArgs
    case args of
      (config:command:[]) -> withConfig config command
      _                   -> showUsage

withConfig :: FilePath -> Command -> IO ()
withConfig cfg cmd = do
    settings <- parseConfig cfg
    execute settings cmd

-- | Get data from YAML. We assumed that there is no need to validate config content.
parseConfig :: FilePath -> IO Settings
parseConfig cfg = do
  t <- readFile cfg
  return $ either throw id $ decodeEither' t

-- | Show generic usage message.
showUsage :: IO ()
showUsage = undefined

-- | execute command from input with settings from config. otherwise, show usage message.
execute :: Settings -> Command -> IO ()
execute settings "ALL" = do
  ranges <- getAllRanges settings
  users <- getAllUsers settings ranges
  printUsers users
execute settings range =
  case validate range of
    (VALID,vrange) -> getUsersFromRange settings vrange >>= printUsers
    (INVALID,_)    -> showUsage

getAllRanges :: Settings -> IO [Range]
getAllRanges settings = undefined

getUsersFromRange :: Settings -> Range -> IO [User]
getUsersFromRange settings range = undefined

getAllUsers :: Settings -> [Range] -> IO [User]
getAllUsers settings ranges = undefined

printUsers :: [User] -> IO ()
printUsers u = undefined

validate :: Command -> (Validation,Range)
validate cmd = undefined

data Settings = Settings String deriving Generic
instance FromJSON Settings

type Command = String

data Validation = VALID | INVALID
data User
data Range