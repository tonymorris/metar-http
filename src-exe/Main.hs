{-# OPTIONS_GHC -Wall #-}

module Main (
  main,
) where

import Data.Aviation.Metar.Http (metarHTTP)

main ::
  IO ()
main =
  metarHTTP
