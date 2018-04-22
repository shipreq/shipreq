# Thoughts

(m) = metric
(l) = label

#### Runtime

- http
  - (m) response times
  - (l) endpoints
  - (l) method
  - (m) response size
  - (l) response code
- SSP
  - (m) response times
  - (l) type
- logins
  - (l) success vs failure
  - unique users
  - forgotten password reqs
- sessions
  - by ip
  - active vs unique
  - duration
- ProjectServer Stores
  - active

#### Content

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

# Real

```
[C] shipreq_http_bytes_total
    x {method, status_code, name, dir=request|response}

[H] shipreq_http_response_duration_seconds
      x {method, status_code, name, delay=none|security}
      / [0.005,0.01,0.025,0.05,0.075,0.1,0.125,0.13,0.145,0.17,0.195,0.22,0.3,0.5,0.75,1,2,3,5,8,12]

[G] shipreq_http_sessions_active

[C] shipreq_http_sessions_total

[G] shipreq_logins_active
      x {type=unique|total}

[C] shipreq_logins_total

[G] shipreq_projects_active

[C] shipreq_secure_request_count
      x {type   = login|forgot_password|reset_password,
         result = success|failure}
```

##### Considered

```
avg session-and/or-login duration per day
[H] shipreq_http_session_duration    - how? running or on-completion?
[H] shipreq_project_duration_seconds - how? running or on-completion?
```
