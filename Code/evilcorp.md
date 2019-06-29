Wants & needs
=============

* PII can't be off-site
* entire team needs access to read
* entire team or subset, needs access to write
* restrict a read-only share to a specified filter - don't allow viewer to see beyond it
* track SDLC status
* track social assignment (eg. current assignee like JIRA, or maybe dev(s) & tester(s))
* track sprint
* story points / estimated days of effort

* scenario: would've been great after the multi-nlp design when I documented prob/solution in markdown,
  then separately created task DAG in graphviz


Win scenarios
=============

* Sprint planning
  * Load once at desk, use offline in meeting (read only)
  * Load once at desk, use offline in meeting, make all changes, take back to desk, save/apply
    * Problem: risky - should also save state to localStorage until save confirmed
    * Problem: no view of unsaved changes - maybe add a page of pending writes
    * Problem: how to save all?
      * does it attempt to save all pending changes on WebSocket reconnection success? Don't think so
      * how can user retry all changes at once
      * what about conflicts lol
    * Problem: can't edit a new requirement that hasn't been saved
  * Add `#TODO{ db to add detail }` tags in meeting, easy to track and fill in once back at desk

