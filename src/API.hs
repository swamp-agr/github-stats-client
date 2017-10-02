{-# LANGUAGE OverloadedStrings #-}
module API where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Lens
import Data.Aeson
import Data.ByteString (ByteString(..))
import qualified Data.Map.Strict as M
import Data.List (intersperse)
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
import System.Process

import Types
import Utils

-- | First goal. Calculate all possible ranges based on total user amounts. 
getAllRanges :: Settings -> IO [Range]
getAllRanges settings = do
  let opts = setCurlOpts settings
  -- get all users amount
  today <- getCurrentTime
  let wholeRange  = getWholeRange today
      allYearRanges  = splitRangeBy 365 wholeRange

  countByYear <- callRepeatedly [] opts allYearRanges
  let (yearRanges, tbdYearRanges) = spanRanges $ filter ((/= 0) . snd) countByYear
      allMonthRanges = concat $ fmap (splitRangeBy 30) tbdYearRanges

  countByMonth <- callRepeatedly [] opts allMonthRanges
  let (monthRanges, tbdMRanges) = spanRanges $ filter ((/= 0) . snd)  countByMonth
      allWeekRanges = concat $ fmap (splitRangeBy 7) tbdMRanges

  countByWeek <- callRepeatedly [] opts allWeekRanges
  let (weekRanges, tbdWRanges) = spanRanges $ filter ((/= 0) . snd)  countByWeek
      allDayRanges = concat $ fmap (splitRangeBy 1) tbdWRanges

  countByDay <- callRepeatedly [] opts allDayRanges
  let (dayRanges, tbdDRanges) = spanRanges $ filter ((/= 0) . snd) countByDay

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

-- | iteratively make a API callCurl (since wreq has an issue with timeout) for list of various inputs to produce the list of responses with HTTP interacting logic.
callRepeatedly :: [(Range,Int)] -> CurlOptions -> [Range] -> IO [(Range,Int)]
callRepeatedly rs copts [] = return $! rs
callRepeatedly rs copts xs = do
  (rng,opts2,i) <- getUsersCountByRange copts (head xs)
  _ <- return $! ""
  callRepeatedly (rng : rs) opts2 $! tail xs
  
  
  

spanRanges :: [(Range, Int)] -> ([Range], [Range])
spanRanges x = (fmap fst $ filter ((<= 1000) . snd) x, fmap fst $ filter (not . (<= 1000) . snd) x)

showWarning :: [Range] -> String
showWarning x = unlines . fmap ((<> " period had a massive users' load!") . show) $ x

getUsersCountByRange :: CurlOptions -> Range -> IO ((Range, Int),CurlOptions,Interaction)
getUsersCountByRange opts rng = do
  (rs,opts2,i) <- getCountRequest opts rng
  let tc = totalCount rs
  putStrLn $ "For Range " <> (show rng) <> " created: " <> (show tc) <> " users!"
  return ((rng, tc),opts2,i)

getCountRequest :: CurlOptions -> Range -> IO (GithubResponse,CurlOptions,Interaction)
getCountRequest opts rng =
  let nOpts = addParam ("per_page","1") $ addParam ("page","1") opts
  in callCurl nOpts rng 

callWreq :: (URL,Options) -> Range -> IO (GithubResponse,Options,Interaction)
callWreq (url,opts) rng = do
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

callCurl :: CurlOptions -> Range -> IO (GithubResponse,CurlOptions,Interaction)
callCurl copts rng = do
  let q  = (M.fromList $ cParam copts) M.! "q"
      rx = mkRegex "([0-9]{4}-[0-9]{2}-[0-9]{2}\\.\\.[0-9]{4}-[0-9]{2}-[0-9]{2})"
      q2 = subRegex rx q (show rng)
      f y x = y
      cnopts = copts { cParam = (M.toList . (M.adjust (f q2) "q") . M.fromList . cParam $ copts) }
      cmd = getCmdString cnopts
  fullResp <- readProcess "curl" cmd []
  let (respHeaders,respBody) = span (/= '{') fullResp
      res = decode $ s2lbs respBody
  case res of
    Just decResult -> return (decResult,copts,di)
    Nothing -> return (defaultResponse, copts, di)
   

getUsersByRange :: Settings -> Range -> IO [User]
getUsersByRange settings rng =
  let opts = setCurlOpts settings
      nOpts = addParam ("per_page","100") opts
  in do
      (cpu,_,_) <- callPages [] 1 nOpts rng
      return cpu

callPages :: [User] -> Int -> CurlOptions -> Range -> IO ([User], CurlOptions, Interaction)
callPages us n opts rng =
  if length us < 10
  then do
    let newOpts = updateParam "page" (show n) opts
    (ghr, opts2, i) <- callCurl newOpts rng
    threadDelay 2000000
    _ <- return $! ""
    callPages (us ++ items ghr) (n+1) opts2 rng
  else do
    let newOpts = updateParam "page" (show n) opts
    (ghr, opts2, i) <- callCurl newOpts rng
    threadDelay 2000000
    return (us ++ items ghr, opts2, i)
  
  

setWreqOpts :: Settings -> (URL,Options)
setWreqOpts settings = 
  let opts = defaults & manager .~ Left (tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro 100000000 } )
             & auth ?~ oauth2Token (encodeUtf8 $ token settings)
             & header "User-Agent" .~ [encodeUtf8 $ ua settings]
             & header "Accept" .~ [encodeUtf8 $ constHeader settings]
             & param "q" .~ [ "location%3A" <> (location settings)
                            <> "+type%3Auser+created%3A2016-01-01..2017-01-01"]
             & param "sort" .~ ["created"]
      url = githubUrl -- <> (unpack $ "?q=location:" <> (location settings) <> "+type:user+created:2016-01-01..2017-01-01")
 in (url,opts)

setCurlOpts :: Settings -> CurlOptions
setCurlOpts (Settings l t ua ch) =
  CurlOptions
    { cHeader = [ ("User-Agent", unpack ua)
                , ("Accept", unpack ch)
                , ("Authorization", "token " <> unpack t)
                ]
    , cParam  = [ ("q", "location:" <> (unpack l) <> "+type:user+created:2006-01-01..2007-01-01")
                , ("sort", "created")
                ]
    , cUrl = githubUrl
    }


addParam param opts = opts { cParam = (param : cParam opts ) }
updateParam paramName newValue opts = opts { cParam = M.toList . (M.adjust (f newValue) paramName) . M.fromList . cParam $ opts }
  where f y x = y

getCmdString :: CurlOptions -> [String]
getCmdString (CurlOptions ch cp cu) = 
  let getHeader (x,y) = x <> ": " <> y
      getParam (x,y)  = x <> "=" <> y

      getFullUrl url ps = url <> "?" <> (concat $ intersperse "&" ps)
  in ("-H" : (intersperse "-H" $ fmap (getHeader) ch)) ++ ("-i" : (getFullUrl cu (fmap getParam cp)) : [])