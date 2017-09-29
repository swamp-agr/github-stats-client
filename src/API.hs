module API where

import Control.Exception
import Data.ByteString (ByteString(..))
import Network.Wreq

import Types

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