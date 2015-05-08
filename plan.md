Phase 2
=======

### ReqTable

* Refs in ReqTitles
* Apply ReqTypes-per-field rules
* ReqCode Refs
  * parse
  * display
  * auto-complete
* Show deleted
  * toggle
  * filter
  * effect on displayed refs
  * effect on auto complete
  * effect on validation
* Handle no rows
* Table status bar
* New req
  * Template
  * Functionality
* Filters
* Issues
  * Highlight empty cells
  * Highlight imp required
  * Tag enum violations
* Bulk
  * Row selection
  * Copy & paste
  * Change reqcode prefix
  * Deleting/Restoring Reqs
    * Data representation + derivation
    * Tree n-level checkboxes
    * Component
* Row detail view (modal?)
* IO
  * Modification protocol
  * Async update
  * Cell locking
  * Failure handling


### Other

* Real project storage
  * Constraints & validation
  * DB & schema
* Cfg screens & usage/deleted
  * Count usage
  * Show usage
  * Prevent deletion
* Loose issue
* Issues screen
  * Screen (composite of views, filter, buttons, summary)
  * Distribution view & data representation
  * Detail view section: Blank
  * Detail view section: Custom
  * Detail view section: Loose
  * Detail view section: Cfg issues


================================================================================

* From ideas.md:
    TODOs due to MF intersection problems.
    where/how to store intersection problems?
    Neither loose nor in-req incmps seem to be enough for intersection probs.

* Maybe its time to revisit the UX book(s).

* Field based on implication just like they have with groupings. Except it
  should be read-only and show resolve transitively to a given req-type.
  So I can create a field showing the driving MF for everything.

* Implications from UC steps ← how?
* Implications from UC fields ← allow?
* Implications from LL fields ← no!

