{-# LANGUAGE OverloadedStrings #-}
module API where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Lens
import Data.Aeson
import Data.ByteString (ByteString(..))
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Time.Clock
import Data.Time.Calendar
import Network.Wreq
import Text.Regex
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import OpenSSL.Session (context)
import Network.HTTP.Client.TLS
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout, responseTimeoutMicro)

import Types
import Utils

-- | First goal. Calculate all possible ranges based on total user amounts. 
getAllRanges :: Settings -> IO [Range]
getAllRanges settings = do
  let opts = setOpts settings
  -- get all users amount
  today <- getCurrentTime
  let wholeRange  = getWholeRange today
      allYearRanges  = splitRangeBy 365 wholeRange

  countByYear <- callRepeatedly [] opts allYearRanges
  let (yearRanges, tbdYearRanges) = spanRanges countByYear
      allMonthRanges = concat $ fmap (splitRangeBy 30) tbdYearRanges

  countByMonth <- callRepeatedly [] opts allMonthRanges
  let (monthRanges, tbdMRanges) = spanRanges countByMonth
      allWeekRanges = concat $ fmap (splitRangeBy 7) tbdMRanges

  countByWeek <- callRepeatedly [] opts allWeekRanges
  let (weekRanges, tbdWRanges) = spanRanges countByWeek
      allDayRanges = concat $ fmap (splitRangeBy 1) tbdWRanges

  countByDay <- callRepeatedly [] opts allDayRanges
  let (dayRanges, tbdDRanges) = spanRanges countByDay

  putStrLn $ showWarning tbdDRanges

  return $ yearRanges ++ monthRanges ++ weekRanges ++ dayRanges

getWholeRange :: UTCTime -> Range
getWholeRange t = Range defDay t

splitRangeBy :: Days -> Range -> [Range]
splitRangeBy t (Range start end) =
  let startDay = utctDay start
      endDay   = utctDay end
      days     = diffDays endDay startDay
      toRangeProto val ix = (ix * val, (ix + 1) * val - 1)
      ixs = div days t
      protoList :: [(Days, Days)]
      protoList = fmap (toRangeProto t) [0..ixs]
      shiftRange :: Day -> (Days,Days) -> Range
      shiftRange s (x,y) = Range (dayToDate $ addDays x s) (dayToDate $ addDays y s)
  in fmap (shiftRange startDay) protoList

-- | iteratively make a API call for list of various inputs to produce the list of responses with HTTP interacting logic.
callRepeatedly :: [(Range,Int)] -> (URL,Options) -> [Range] -> IO [(Range,Int)]
callRepeatedly rs (url,opts) [] = return $! rs
callRepeatedly rs (url,opts) xs = do
  (rng,opts2,i) <- getUsersCountByRange (url,opts) (head xs)
  _ <- return $! ""
  callRepeatedly (rng : rs) (url,opts2) $! tail xs
  
  
  

spanRanges :: [(Range, Int)] -> ([Range], [Range])
spanRanges x = (fmap fst $ filter ((<= 1000) . snd) x, fmap fst $ filter (not . (<= 1000) . snd) x)

showWarning :: [Range] -> String
showWarning x = unlines . fmap ((<> " period had a massive users' load!") . show) $ x

getUsersCountByRange :: (URL,Options) -> Range -> IO ((Range, Int),Options,Interaction)
getUsersCountByRange (url,opts) rng = do
  (rs,opts2,i) <- getCountRequest (url,opts) rng
  let tc = totalCount rs
  putStrLn $ "For Range " <> (show rng) <> " created: " <> (show tc) <> " users!"
  return ((rng, tc),opts2,i)

getCountRequest :: (URL,Options) -> Range -> IO (GithubResponse,Options,Interaction)
getCountRequest (url,opts) rng =
  let nOpts = opts & param "per_page" .~ ["1"] & param "page" .~ ["1"]
  in call (url,nOpts) rng 

call :: (URL,Options) -> Range -> IO (GithubResponse,Options,Interaction)
call (url,opts) rng = do
  let q = (M.fromList $ opts ^. params) M.! "q"
      rx = mkRegex "([0-9]{4}-[0-9]{2}-[0-9]{2}\\.\\.[0-9]{4}-[0-9]{2}-[0-9]{2})"
      q2 = pack $ subRegex rx (unpack q) (show rng)
      nOpts = opts & param "q" .~ [q2]
      nurl = subRegex rx url (show rng)
  resp <- try $ asJSON =<< getWith nOpts nurl
  case resp of
     Right result -> do
       let i = Interaction (parseInt $ decodeUtf8 $ result ^. responseHeader "X-RateLimit-Limit") (parseInt $ decodeUtf8 $ result ^. responseHeader "X-RateLimit-Remaining") (parseInteger $ decodeUtf8 $ result ^. responseHeader "X-RateLimit-Reset")
       return (result ^. responseBody, nOpts, i)
     Left (SomeException _) -> do
       putStrLn $ "ERROR for " <> (show rng)
       return (defaultResponse, nOpts,di)
  
getUsersByRange :: Settings -> Range -> IO [User]
getUsersByRange settings rng =
  let (url,opts) = setOpts settings
      nOpts = opts & param "per_page" .~ ["100"] 
  in do
      (cpu,_,_) <- callPages [] 1 (url,nOpts) rng
      return cpu

callPages :: [User] -> Int -> (URL,Options) -> Range -> IO ([User], Options, Interaction)
callPages us n (url,opts) rng =
  if length us < 10
  then do
    let newOpts = opts & param "page" .~ [pack . show $ n]
    (ghr, opts2, i) <- call (url,newOpts) rng
    threadDelay 2000000
    _ <- return $! ""
    callPages (us ++ items ghr) (n+1)  (url,opts2) rng
  else do
    let newOpts = opts & param "page" .~ [pack . show $ n]
    (ghr, opts2, i) <- call (url,newOpts) rng
    threadDelay 2000000
    return (us ++ items ghr, opts2, i)
  
  

setOpts :: Settings -> (URL,Options)
setOpts settings = 
  let opts = defaults & manager .~ Left (tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro 100000000 } )
             & auth ?~ oauth2Token (encodeUtf8 $ token settings)
             & header "User-Agent" .~ [encodeUtf8 $ ua settings]
             & header "Accept" .~ [encodeUtf8 $ constHeader settings]
             & param "q" .~ [ "location%3A" <> (location settings)
                            <> "+type%3Auser+created%3A2016-01-01..2017-01-01"]
             & param "sort" .~ ["created"]
      url = githubUrl -- <> (unpack $ "?q=location:" <> (location settings) <> "+type:user+created:2016-01-01..2017-01-01")
 in (url,opts)

invokeCurl = undefined