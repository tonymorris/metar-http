{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Metar.Http(
  metarHTTP
, metarHTTPapp
) where

import Control.Applicative(pure)
import Control.Category((.), id)
import Control.Lens((^.), (^?), _Wrapped, folded)
import Control.Monad((>>=))
import Data.Aviation.Metar(getAllMETAR, getAllTAF)
import Data.Aviation.Metar.TAFResult(_TAFResultValue)
import Data.ByteString.Lazy.UTF8 hiding (take)
import Data.Functor((<$>))
import Data.Function(($))
import Data.List(intercalate, take)
import Data.Maybe(Maybe(Nothing, Just), fromMaybe)
import Data.String(String)
import Data.Semigroup((<>))
import Data.Text(unpack, toLower)
import Data.Tuple(fst)
import Network.HTTP.Types.Header(hContentType)
import Network.HTTP.Types.Status(status404, status200)
import Network.Wai(Application, responseLBS, pathInfo)
import Network.Wai.Handler.Warp(setPort, runSettings, defaultSettings)
import System.Environment(getArgs)
import System.IO(IO)
import Text.Read(Read, reads)
import Text.Show(show)

readMaybe ::
  Read a =>
  String
  -> Maybe a
readMaybe n =
  fst <$> reads n ^? folded

metarHTTPapp ::
  Application
metarHTTPapp req withResp =
  let msg =
        "path /metar/<icao> OR /metar/<icao>/<nlines> OR /taf/<icao> OR /taf/<icao>/<nlines> OR /metar/<icao>/* OR /taf/<icao>/*"
      _404 =
        responseLBS
          status404
          []
          msg
  in  case pathInfo req of
        (rpt:xxxx:r) ->
          let xxxx' =
                unpack xxxx
              tk r' =
                case unpack <$> r' of
                  "":_ ->
                    Just id
                  "*":z ->
                    let z' =
                          z ^? folded >>=
                          readMaybe >>=
                          pure . take
                    in  Just (\k -> [fromMaybe id z' (intercalate " " k)])
                  n:_ ->
                    take <$> readMaybe n
                  [] ->
                    Just id
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
                  case tk r of
                    Nothing ->
                      withResp $
                        responseLBS
                          status404
                          []
                          ("not a valid number " <> fromString (show r))
                    Just q ->
                      do  t <- (intercalate "\n" . q <$> mtf) ^. _Wrapped
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
                                  (fromString x)
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
      runSettings (p defaultSettings) metarHTTPapp
