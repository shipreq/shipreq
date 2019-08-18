### FATAL

Errors that cause the application to crash or fail to start

Eg. JVM out of memory

### ERROR

Problems that need to be actionable by someone.
These should page ops staff or trigger automated routines.

Eg. couldn't connect to DB

### WARN

Temporary problems or unexpected behavior that doesn't significantly hamper the functioning of the application.
Also failed security events.
These would appear on ops console and might be monitored for volume.

Eg. couldn't connect to DB but will retry
Eg. failed login due to a bad password
Eg. bad user inputs

### INFO

Messages that describe what's happening in the application.
Every single call to an external dependency (ideally with timing).
These should provide a high level map of what's going on with the app.

Eg. user registered
Eg. saved file
Eg. called DB

### DEBUG

Messages that could be useful in debugging an issue.

### TRACE

As above but way more detail, or size of log content.


# Summary

FATAL - going down
ERROR - call someone
INFO  - what's going on
DEBUG - event detail
TRACE - data detail
