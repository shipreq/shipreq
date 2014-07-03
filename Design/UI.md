# Fields
  * Create
    * Type: text, grouping, courses, flow graph (not editable later)
  * Edit
    * Mandatory (bool)
    * Applicable req-types: all | whitelist | blacklist
    * Type-specific attributes

# Add loose incompletion
  * Specify: type & text.

## Groupings (specific)
  * ∀ s : bar graph segment. list of reqs in s
    * Select/deselect req
    * Apply subject or child grouping to selected reqs.
    * Remove subject or child grouping from selected reqs.
    * Replace subject or child grouping in selected reqs, with 1 or more groupings of any kind.
  * Add child grouping
  * Edit subject or child grouping
  * Delete subject or child grouping
  * Restore child grouping
## Implication sources (specific req type)
  * ∀ s : bar graph segment. list of reqs implied by s
    * Select/deselect req
    * Apply implication from a req of subject req type, to selected reqs.
    * Remove implication from a req of subject req type, from selected reqs.
    * Replace implication from a req of subject req type, in selected reqs, with 1 or more others.
  * Add req of subject req type
  * Edit subject req type
  * Delete subject req type

# Requirement Types
  * Props: implication required, mnemonic, name
  * Create
  * List
  * Edit
  * Delete
    * Usage summary (number(s) or table)
  * Bin
    * Restore

# Req List
  * Toggle visibility of deleted reqs
  * Create: any req type, SHR
  * Specify implying req(s) for new req creations
  * ∀reqs. selected, ID, semIDs, desc/name, anything else in sort criteria
    * Maybe: revision & last-updated time & user, implication src/tgt
  * Selected reqs
    * Create child
    * Restore deleted
      * List of implied reqs to also restore.
    * Create copy
    * Change req-type
    * Change common sem-ID prefix
  * Edit
    * View/Edit text tokens
    * Implications.
    * Groupings.
  * Delete
    * Usage summary (number(s) or table) of incoming references
    * Usage detail: Show each incoming references
    * List of implied reqs to also delete.
  * Filter.
  * Sort criteria: view, edit, reset

# Text Tokens
  * Create
  * List & View
  * Edit
  * Delete
    * Usage summary (number(s) or table)
    * Usage detail: Show each req field using it

================================================================================
Common

# Filter
  * Filter by grouping
  * Filter by incmp type
  * Filter by req type
  * Filter by implying reqs
  * Filter by text

