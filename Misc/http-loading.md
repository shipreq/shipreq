async
- JS only
- download in background
- execute as soon as downloaded, pausing HTML parsing
- not guaranteed to execute in specified order
- blocks onload

defer
- JS only
- download in background
- complete HTML parsing before JS execution
- defered scripts all executed in declaration order
- blocks DOMContentLoaded

preconnect
- init connection immediately (dns/tcp/tls)
- use connection (i.e. download) when required

prefetch
- low pri
- loads asset, stores in cache
- no effect on page or load events

preload
- v2 of async/defer for any data type
- doesn't block onload (unless the resource is also requested by a resource that blocks that event)

