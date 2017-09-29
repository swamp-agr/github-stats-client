module CLI where

import Control.Exception
import Data.ByteString (ByteString(..), readFile)
import Data.Yaml
import Prelude hiding (readFile)

import Types
import API

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

printUsers :: [User] -> IO ()
printUsers u = mapM_ printUser u
  where printUser = putStrLn . show

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

validate :: Command -> (Validation,Range)
validate cmd = undefined