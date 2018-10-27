{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import System.IO(IO)
import Data.Aviation.Metar.Http(metarHTTP)

main ::
  IO ()
main =
  metarHTTP
  