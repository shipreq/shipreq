1. Decide saving strategy, considering:
    * MF-08: History/Audit
    * MF-09: Collaboration: authoring
    * MF-10: Collaboration: stakeholders
    * MF-11: Collaboration: change mgnt & approval
    * MF-17: Undo & Auto-save

1. Determine risk to phase-2 DB from:
    * MF-04: Templates
    * MF-08: History/Audit
    * MF-09: Collaboration: authoring
    * MF-10: Collaboration: stakeholders
    * MF-11: Collaboration: change mgnt & approval
    * MF-17: Undo & Auto-save

2. Plan UI for:
     * MF-01: Use Case Editor
     * MF-05: Field Customisation
     * MF-06: Incompletions
     * MF-07: Organisation
     * MF-12: Low-level Requirements (CO, FR, BR, etc.)
     * MF-13: Requirement Relationships
     * MF-22: High-level Requirements (MF, BO, SC, etc.)

3. Mock up UI.

4. Do prelim analysis and pull out probable changes to phase-2:
    * MF-14: Text-generated Diagrams
    * MF-20: Generic artifact storage (DOC, PDF, PNG, XLS)

4. ¿Split subsequent requirements with coding?

================================================================================

* Typing groupings whilst editing ← not analysed properly.
  System behaviour changes when typed into built-in field vs custom field.
  In one case it applies to the entire req; in the other, the field contents.

* From ideas.md:
    TODOs due to MF intersection problems.
    where/how to store intersection problems?
    Neither loose nor in-req incmps seem to be enough for intersection probs.

* Work out issues with subreqs.

* Reqs around min/max time auto-save bounds. Not too quick, don't wait too long.

* Reqs for undo. Undo(undo)? Have to pull MF-17 into phase 2 scope...
  - Connection problems.
  - Validation problems. (Eg. cyclic structs, etc.)
    Delayed saving probably fine but validation is better asap.
  - Knockon effects such as add/del/reind step handled how?
    Am I conflating saving + history concerns with multi-user anti-conflict
    concerns? Seems like it.
  - Actually MF-17 shouldn't be a problem until there are multiple users (in a
    single-user ctx it only affects multiple tabs). Maybe simplest would be best
    until phase 3? Consider history reqs for data structures but whack save
    buttons on things and/or auto-save (LL-reqs) where appropriate for now.
    UX inconsistency but maybe ok short-term, changability cost shouldn't be high.

* Maybe its time to revisit the UX book(s).

