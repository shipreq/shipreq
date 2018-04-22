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
[C]ounter   - monotonic number
[G]auge     - number
[H]istogram - stream of numbers
```

# Real

```
[H] shipreq_http_bytes            x {method, status_code, name, dir=request|response}
[H] shipreq_http_duration_seconds x {method, status_code, name}
[G] shipreq_http_sessions_active
[G] shipreq_logins_active         x {type=unique|total}
[G] shipreq_projects_active
[C] shipreq_secure_request_count  x {type   = login|forgot_password|reset_password,
                                     result = success|failure}
```

##### Considered

```
[G] shipreq_http_sessions_active {maybe include type=unique like login, by IP}
[H] shipreq_http_session_duration    - how? running or on-completion?
[H] shipreq_project_duration_seconds - how? running or on-completion?
```
