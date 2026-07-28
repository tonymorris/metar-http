# metar-http

A trivial HTTP server that serves METAR observations, backed by the
[`metar`](https://github.com/tonymorris/metar) library. Australian aerodromes
(ICAO codes beginning with `Y`) are fetched from the Bureau of Meteorology;
other codes fall back to NOAA.

## Running

```
cabal run metar-http            # default port (Warp default: 3000)
cabal run metar-http -- 8080    # custom port
```

Or after `cabal install`:

```
metar-http 8080
```

## Endpoints

```
/                                       help text
/metar/<icao>                           raw METAR (may span multiple lines)
/metar/<icao>/*                         METAR all on one line
/metar/<icao>/*/<maxchars>              one line, truncated to <maxchars>
/metar/<icao>/*/<maxchars>/<append>     one line, truncated with <append> suffix on truncation
/metar/<icao>/<maxlines>                first <maxlines> lines
/metar/<icao>/<maxlines>/<maxchars>     first <maxlines>, truncated to <maxchars>
/metar/<icao>/<maxlines>/<maxchars>/<append>
                                        first <maxlines>, truncated with <append> on truncation
```

Examples:

```
curl http://localhost:8080/metar/YSSY
curl http://localhost:8080/metar/YBAF/*
curl http://localhost:8080/metar/KJFK/1/80/...
```

## Development

```
bin/lint.sh          hlint + fourmolu (check)
bin/lint.sh --fix    apply fixes
bin/test.sh          run doctests
```
