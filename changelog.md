0.0.5

* Update to metar-0.0.5 library
* Use `getMETAR` (BOM for `Y*` codes, NOAA fallback) instead of NOAA only
* Add hlint and fourmolu configuration
* Add `bin/lint.sh` and `bin/test.sh`
* Add doctests; remove empty test-suite
* Add `{-# OPTIONS_GHC -Wall #-}` to all source files
* Remove `NoImplicitPrelude`

0.0.4

* Update to GHC 9.6.7 compatibility
* Update to metar-0.0.4 library (uses getNOAAMETAR)
* Remove TAF support (not available in metar-0.0.4)
* Relax dependency upper bounds for modern GHC

0.0.3

* refactor of code to allow more options
* allow maxchars option

0.0.2

* allow .../* request for all on one line

0.0.1

* Initial release
