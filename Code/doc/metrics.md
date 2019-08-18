# Thoughts

(m) = metric
(l) = label

#### Content

These needn't come from webapp...

- users
  - registered vs not
  - last login
- projects
  - total created & deleted
- requirements
  - total
  - per project
- events
  - total
  - per project
- disk space
  - total
  - per project
  - per user
  - per event
  - per requirement


# Cheatsheet

```
[C]ounter   - monotonic number; use for X per N (eg. req/sec)
[G]auge     - number
[H]istogram - stream of numbers, recorded in buckets
[S]ummary   - client-side quantiles, pre-determined window size
```
