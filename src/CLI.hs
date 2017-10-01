module CLI where

import Control.Exception
import Data.ByteString (ByteString(..), readFile)
import Data.Yaml
import Prelude hiding (readFile)
import Text.Regex

import Types
import API
import Utils

withConfig :: FilePath -> Command -> IO ()
withConfig cfg cmd = do
    settings <- parseConfig cfg
    execute settings cmd

-- | Get data from YAML. We assumed that there is no need to validate config content.
parseConfig :: FilePath -> IO Settings
parseConfig cfg = do
  t <- readFile cfg
  return $ either throw id $ decodeEither' t

-- | Show generic usage message. For simplicity, only dates supported.
showUsage :: IO ()
showUsage = putStrLn "github-stats-client [ ALL | <RANGE> ] CONFIG\n\ne.g.\n\tgithub-stats-client ALL config.yml\n\tgithub-stats-client 2017-01-01..2017-09-01 config.yml"

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
      gr <- getGithubResponseFromRange settings vrange
      printUsers $ items gr 
    (INVALID,_)    -> showUsage

validate :: Command -> (Validation,Range)
validate cmd = 
  case ms of
    Nothing -> (INVALID, defRange)
    Just  x -> toRange x
  where rx = mkRegex "^([0-9]{4}-[0-9]{2}-[0-9]{2})\\.\\.([0-9]{4}-[0-9]{2}-[0-9]{2}$)"
        ms = matchRegex rx cmd

