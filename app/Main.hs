{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Exception
import Data.Yaml
import Data.ByteString (ByteString(..), readFile)
import Prelude hiding (readFile)
import System.Environment
import GHC.Generics
import Network.Wreq

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
    (VALID,vrange) -> do
      users <- getUsersFromRange settings vrange
      printUsers users
    (INVALID,_)    -> showUsage

getAllRanges :: Settings -> IO [Range]
getAllRanges settings = undefined

getUsersFromRange :: Settings -> Range -> IO [User]
getUsersFromRange settings range = do
  let opts = setOpts settings
  resp <- try $ call opts range
  case resp of
    Right result -> return $ decodeUsers result
    Left err@(SomeException _)     -> return []

getAllUsers :: Settings -> [Range] -> IO [User]
getAllUsers settings ranges = do
  let opts = setOpts settings
  responses <- callMany opts ranges
  return $ concat $ fmap fromEither responses

printUsers :: [User] -> IO ()
printUsers u = undefined

validate :: Command -> (Validation,Range)
validate cmd = undefined

call :: Options -> Range -> IO ByteString
call opts range = undefined

callMany :: Options -> [Range] -> IO [APIResponse]
callMany opts ranges = undefined

decodeUsers :: ByteString -> [User]
decodeUsers resp = undefined

setOpts :: Settings -> Options
setOpts settings = undefined

fromEither :: APIResponse -> [User]
fromEither (Right usr) = usr
fromEither (Left _) = []

data Settings = Settings String deriving Generic
instance FromJSON Settings

type Command = String

data Validation = VALID | INVALID
data User
data Range
data APICall

type APIResponse = Either ErrorMessage [User]
type ErrorMessage = ByteString