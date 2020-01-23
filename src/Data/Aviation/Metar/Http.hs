{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Metar.Http(
  metarHTTP
, metarHTTPapp
) where

import Control.Category((.), id)
import Control.Lens((^.), (^?), _Wrapped, folded)
import Data.Aviation.Metar(getAllMETAR, getAllTAF)
import Data.Aviation.Metar.TAFResult(_TAFResultValue)
import Data.ByteString.Lazy.UTF8 hiding (take, splitAt)
import Data.Eq(Eq)
import Data.Functor((<$>))
import Data.Function(($))
import Data.Int(Int)
import Data.List(intercalate, take, splitAt)
import Data.Maybe(Maybe(Nothing, Just))
import Data.String(String)
import Data.Semigroup((<>))
import Data.Text(unpack, toLower)
import Data.Tuple(fst)
import Network.HTTP.Types.Header(hContentType)
import Network.HTTP.Types.Status(status404, status200)
import Network.Wai(Application, responseLBS, pathInfo)
import Network.Wai.Handler.Warp(setPort, setTimeout, runSettings, defaultSettings)
import System.Environment(getArgs)
import System.IO(IO)
import Text.Read(Read, reads)
import Text.Show(Show)

readMaybe ::
  Read a =>
  String
  -> Maybe a
readMaybe n =
  fst <$> reads n ^? folded

data CharLimit =
  NoCharLimit
  | MaxChars Int
  | MaxCharsAppend Int String
  deriving (Eq, Show)

charLimit ::
  CharLimit
  -> String
  -> String
charLimit m s =
  case m of
    NoCharLimit ->
      s
    MaxChars n ->
      take n s
    MaxCharsAppend n l ->
      let (a, b) =
            splitAt n s
          b' =
            case b of
              [] ->
                []
              _:_ ->
                l
      in  a <> b'

data Format =
  Raw
  | MaxLines Int CharLimit
  | AllOneLine CharLimit
  deriving (Eq, Show)

-- |
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
-- >>> format (AllOneLine (MaxCharsAppend 120 "abc")) ["METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK","RF00.0/000.4"]
-- "METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK RF00.0/000.4"
--
-- >>> format (AllOneLine (MaxCharsAppend 80 "abc")) ["METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK","RF00.0/000.4"]
-- "METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK RF00.0/000.4"
-- 
-- >>> format (AllOneLine (MaxCharsAppend 60 "abc")) ["METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK","RF00.0/000.4"]
-- "METAR YBAF 071230Z AUTO 16006KT 9999 // NCD 24/20 Q1011 RMK abc"
format ::
  Format
  -> [String]
  -> String
format f s =
  let limitCalate l x =
        charLimit l . intercalate x
  in  case f of
        Raw ->
          intercalate "\n" s
        MaxLines n l ->
          limitCalate l "\n" . take n $ s
        AllOneLine l ->
          limitCalate l " " s

--          Raw
-- *        AllOneLine NoCharLimit
-- */n      AllOneLine (MaxChars n)
-- */n/xyz  AllOneLine (MaxCharsAppend n xyz)
-- n        MaxLines n NoCharLimit
-- n/m      MaxLines n (MaxChars m)
-- n/m/xyz  MaxLines n (MaxCharsAppend m xyz)
uriPathFormat ::
  [String]
  -> Format
uriPathFormat [] =
  Raw
uriPathFormat (q:r) =
  let rawMaybe ::
        Read a =>
        (a -> CharLimit)
        -> String
        -> CharLimit
      rawMaybe f n =
        case readMaybe n of
          Nothing ->
            NoCharLimit
          Just c ->
            f c
      r' =
        case r of
          [] ->
            NoCharLimit
          s:ss ->
            rawMaybe (\n -> case ss of
                              [] ->
                                MaxChars n
                              t:_ ->
                                MaxCharsAppend n t) s
  in  case q of
        "*" ->
          AllOneLine r'
        _ ->
          case readMaybe q of
            Nothing ->
              Raw
            Just l ->
              MaxLines l r'

metarHTTPapp ::
  Application
metarHTTPapp req withResp =
  let msg =
        let a </> b =
              a <> "\n" <> b
            a <//> b =
              a </> "\n" <> b
        in  "/metar/<icao>" </>
            "raw metar for station <icao>" <//>
            "/metar/<icao>/*" </>
            "metar for station <icao> all on one line" <//>
            "/metar/<icao>/*/<maxchars>" </>
            "metar for station <icao> all on one line truncated at <maxchars>" <//>
            "/metar/<icao>/*/<maxchars>/<appendstr>" </>
            "metar for station <icao> all on one line truncated at <maxchars> and if truncation occurs, append <appendstr>" <//>
            "/taf/<icao>" </>
            "raw taf for station <icao>" <//>
            "/taf/<icao>/*" </>
            "taf for station <icao> all on one line" <//>
            "/taf/<icao>/*/<maxchars>" </>
            "taf for station <icao> all on one line truncated at <maxchars>" <//>
            "/taf/<icao>/*/<maxchars>/<appendstr>" </>
            "taf for station <icao> all on one line truncated at <maxchars> and if truncation occurs, append <appendstr>" <//>
            ""
      _404 =
        responseLBS
          status404
          []
          msg
  in  case pathInfo req of
        (rpt:xxxx:r) ->
          let xxxx' =
                unpack xxxx
              modifyOutput ::
                [String]
                -> String
              modifyOutput =
                format (uriPathFormat (unpack <$> r))
              mt =
                case toLower rpt of
                  "metar" ->
                    Just ("METAR", getAllMETAR xxxx')
                  "taf" ->
                    Just ("TAF", getAllTAF xxxx')
                  _ ->
                    Nothing
          in  case mt of
                Nothing ->
                  withResp _404
                Just (mtt, mtf) ->
                  do  t <- mtf ^. _Wrapped
                      withResp $
                        case t ^? _TAFResultValue of
                          Nothing -> 
                            responseLBS
                              status404
                              []
                              ("no " <> mtt <> " found for " <> fromString xxxx')
                          Just x ->
                            responseLBS
                              status200
                              [(hContentType, "text/plain")]
                              (fromString (modifyOutput x))
        [] ->
          withResp $
            responseLBS
              status200
              [(hContentType, "text/plain")]
              msg
        _ ->
          withResp _404

metarHTTP ::
  IO ()
metarHTTP =
  do  a <- getArgs
      let p =
            case a of
              [] ->
                id
              (q:_) ->
                case readMaybe q of
                  Nothing ->
                    id
                  Just n ->
                    setPort n
      runSettings (setTimeout 6 (p defaultSettings)) metarHTTPapp
