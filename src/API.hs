module API where

import Control.Exception
import Data.ByteString (ByteString(..))
import Network.Wreq

import Types

-- | First goal. Calculate all possible ranges based on total user amounts. 
getAllRanges :: Settings -> IO [Range]
getAllRanges settings = do
  let opts = setOpts settings
  -- get all users amount 
  count <- getUsersCount settings
  -- calculate length of list for future ranges
  let rl = (+ 1) $ truncate $ (/ 1000) $ fromIntegral count
  -- naive approach
  return undefined
  
getUsersCount settings = do
  rs <- getGithubResponseFromRange settings defRange
  return $ totalCount rs

getGithubResponseFromRange :: Settings -> Range -> IO GithubResponse
getGithubResponseFromRange settings range = do
  let opts = setOpts settings
  resp <- try $ call opts range
  case resp of
    Right result -> return $ decodeGithubResponse result
    Left err@(SomeException _)     -> return defaultResponse

getAllUsers :: Settings -> [Range] -> IO [User]
getAllUsers settings ranges = do
  let opts = setOpts settings
  rs <- callMany opts ranges
  return $ concat $ fmap fromEither rs

call :: Options -> Range -> IO ByteString
call opts range = undefined

callMany :: Options -> [Range] -> IO [APIResponse]
callMany opts ranges = undefined

decodeGithubResponse :: ByteString -> GithubResponse
decodeGithubResponse resp = undefined

setOpts :: Settings -> Options
setOpts settings = undefined

fromEither :: APIResponse -> [User]
fromEither (Right x) = items x
fromEither (Left _) = []