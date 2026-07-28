{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | HTTP server exposing METAR observations. Australian aerodromes (ICAO codes
-- beginning with @Y@) are fetched from BOM; other codes fall back to NOAA.
module Data.Aviation.Metar.Http (
  metarHTTP,
  metarHTTPapp,
) where

import Control.Lens (folded, (^.), (^?), _Wrapped)
import Data.Aviation.Metar (getMETAR)
import Data.Aviation.Metar.METARResult (_METARResultValue)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.List (intercalate)
import Data.Text (toLower, unpack)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (status200, status404)
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort, setTimeout)
import System.Environment (getArgs)

{- FOURMOLU_DISABLE -}
-- $setup
-- >>> import Data.Aviation.Metar.Http
{- FOURMOLU_ENABLE -}

-- | Parse a value using its 'Read' instance, returning 'Nothing' on failure.
--
-- >>> readMaybe "42" :: Maybe Int
-- Just 42
--
-- >>> readMaybe "notanumber" :: Maybe Int
-- Nothing
readMaybe ::
  (Read a) =>
  String ->
  Maybe a
readMaybe n =
  fst <$> reads n ^? folded

-- | How to truncate a rendered METAR line.
data CharLimit
  = NoCharLimit
  | MaxChars Int
  | MaxCharsAppend Int String
  deriving (Eq, Show)

-- | Apply a 'CharLimit' to a string.
--
-- >>> charLimit NoCharLimit "hello world"
-- "hello world"
--
-- >>> charLimit (MaxChars 5) "hello world"
-- "hello"
--
-- >>> charLimit (MaxCharsAppend 5 "...") "hello world"
-- "hello..."
--
-- >>> charLimit (MaxCharsAppend 5 "...") "hi"
-- "hi"
charLimit ::
  CharLimit ->
  String ->
  String
charLimit m s =
  case m of
    NoCharLimit ->
      s
    MaxChars n ->
      take n s
    MaxCharsAppend n l ->
      let (a, b) = splitAt n s
          b' = case b of
            [] -> []
            _ : _ -> l
       in a <> b'

-- | How to format a list of METAR lines for a response body.
data Format
  = Raw
  | MaxLines Int CharLimit
  | AllOneLine CharLimit
  deriving (Eq, Show)

-- | Render lines of METAR text according to the given 'Format'.
--
-- >>> format (MaxLines 3 NoCharLimit) ["METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK","RF00.0/000.4"]
-- "METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK\nRF00.0/000.4"
--
-- >>> format (MaxLines 1 NoCharLimit) ["METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK","RF00.0/000.4"]
-- "METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK"
--
-- >>> format (MaxLines 1 (MaxChars 15)) ["METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK","RF00.0/000.4"]
-- "METAR YBAF 0712"
--
-- >>> format (MaxLines 1 (MaxCharsAppend 15 "abc")) ["METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK","RF00.0/000.4"]
-- "METAR YBAF 0712abc"
--
-- >>> format (AllOneLine (MaxCharsAppend 15 "abc")) ["METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK","RF00.0/000.4"]
-- "METAR YBAF 0712abc"
--
-- >>> format (AllOneLine (MaxCharsAppend 150 "abc")) ["METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK","RF00.0/000.4"]
-- "METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK RF00.0/000.4"
--
-- >>> format (AllOneLine (MaxCharsAppend 60 "abc")) ["METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK","RF00.0/000.4"]
-- "METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK abc"
format ::
  Format ->
  [String] ->
  String
format f s =
  let limitCalate l x =
        charLimit l . intercalate x
   in case f of
        Raw ->
          intercalate "\n" s
        MaxLines n l ->
          limitCalate l "\n" . take n $ s
        AllOneLine l ->
          limitCalate l " " s

-- | Parse the trailing URI path components into a 'Format'.
--
-- URI grammar:
--
-- @
-- (empty)   -> Raw
-- *         -> AllOneLine NoCharLimit
-- *\/n       -> AllOneLine (MaxChars n)
-- *\/n\/xyz   -> AllOneLine (MaxCharsAppend n xyz)
-- n         -> MaxLines n NoCharLimit
-- n\/m       -> MaxLines n (MaxChars m)
-- n\/m\/xyz   -> MaxLines n (MaxCharsAppend m xyz)
-- @
--
-- >>> uriPathFormat []
-- Raw
--
-- >>> uriPathFormat ["*"]
-- AllOneLine NoCharLimit
--
-- >>> uriPathFormat ["*", "80"]
-- AllOneLine (MaxChars 80)
--
-- >>> uriPathFormat ["*", "80", "..."]
-- AllOneLine (MaxCharsAppend 80 "...")
--
-- >>> uriPathFormat ["3"]
-- MaxLines 3 NoCharLimit
--
-- >>> uriPathFormat ["3", "40"]
-- MaxLines 3 (MaxChars 40)
--
-- >>> uriPathFormat ["3", "40", "..."]
-- MaxLines 3 (MaxCharsAppend 40 "...")
--
-- >>> uriPathFormat ["notanumber"]
-- Raw
uriPathFormat ::
  [String] ->
  Format
uriPathFormat [] =
  Raw
uriPathFormat (q : r) =
  let rawMaybe ::
        (Read a) =>
        (a -> CharLimit) ->
        String ->
        CharLimit
      rawMaybe f n =
        maybe NoCharLimit f (readMaybe n)
      r' = case r of
        [] ->
          NoCharLimit
        s : ss ->
          rawMaybe
            ( \n -> case ss of
                [] ->
                  MaxChars n
                t : _ ->
                  MaxCharsAppend n t
            )
            s
   in case q of
        "*" ->
          AllOneLine r'
        _ ->
          case readMaybe q of
            Nothing ->
              Raw
            Just l ->
              MaxLines l r'

-- | WAI 'Application' serving METAR observations.
metarHTTPapp ::
  Application
metarHTTPapp req withResp =
  let msg =
        let a </> b =
              a <> "\n" <> b
            a <//> b =
              a </> "\n" <> b
         in "/metar/<icao>"
              </> "raw metar for station <icao>"
              <//> "/metar/<icao>/*"
              </> "metar for station <icao> all on one line"
              <//> "/metar/<icao>/*/<maxchars>"
              </> "metar for station <icao> all on one line truncated at <maxchars>"
              <//> "/metar/<icao>/*/<maxchars>/<appendstr>"
              </> "metar for station <icao> all on one line truncated at <maxchars> and if truncation occurs, append <appendstr>"
              <//> ""
      _404 =
        responseLBS
          status404
          []
          msg
   in case pathInfo req of
        (rpt : xxxx : r) ->
          let xxxx' =
                unpack xxxx
              modifyOutput ::
                [String] ->
                String
              modifyOutput =
                format (uriPathFormat (unpack <$> r))
              mt =
                case toLower rpt of
                  "metar" ->
                    Just ("METAR", getMETAR xxxx')
                  "taf" ->
                    Nothing
                  _ ->
                    Nothing
           in case mt of
                Nothing ->
                  withResp _404
                Just (mtt, mtf) ->
                  do
                    t <- mtf ^. _Wrapped
                    withResp $
                      case t ^? _METARResultValue of
                        Nothing ->
                          responseLBS
                            status404
                            []
                            ("no " <> mtt <> " found for " <> fromString xxxx')
                        Just x ->
                          responseLBS
                            status200
                            [(hContentType, "text/plain")]
                            (fromString (modifyOutput [x]))
        [] ->
          withResp $
            responseLBS
              status200
              [(hContentType, "text/plain")]
              msg
        _ ->
          withResp _404

-- | Run 'metarHTTPapp' with Warp, optionally taking a port from the first
-- command-line argument.
metarHTTP ::
  IO ()
metarHTTP =
  do
    a <- getArgs
    let p = case a of
          [] ->
            id
          (q : _) ->
            maybe id setPort (readMaybe q)
    runSettings (setTimeout 6 (p defaultSettings)) metarHTTPapp
