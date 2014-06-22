Done: 5,6,7,12,13

# Fields
  * List
  * Reorder
  * Create
    * Type: text, grouping, courses, flow graph (not editable later)
  * Edit
    * Mandatory (bool)
    * Applicable req-types: all | whitelist | blacklist
    * Type-specific attributes
  * Delete
    * Usage summary (number(s) or table)
    * Usage detail: Show each instance
  * Bin
    * Restore

# Incompletions
  * Total number.
  * Filter by grouping, incmp type, req type, implying reqs
  * ∀req. req id, title, incmp count
  * ∀incmp. req id, name, field name, incmp supp info, incmp context

# Add loose incompletion

# Incompletion types & config
  * Create
  * ∀incmp type.
    * Edit
    * Delete
      * Usage summary (number(s) or table)
      * Usage detail: Show each instance
  * Specify all req-types requiring implication

# Grouping types
  * List as tree
  * Edit
    * Children are mutually-exclusive
    * Parents
    * Children
  * Create
    * Type: Applicable or header (not editable later)
  * Reorder
  * Delete
    * Usage summary (number(s) or table)
    * Usage detail: Show each instance
  * Bin
    * Restore

# Groupings (top-level)
  * Filter by grouping, incmp type, req type, implying reqs
  * Top-level Groupings
  * Allocation graphs
    * Segments = children of subject grouping
    * Detail of children: name, application count, %.
  * Change scope to: child grouping
# Implication sources (top-level)
  * Filter by grouping, incmp type, req type, implying reqs
  * Req types that don't require implication (eg. MF)
  * Implication graphs
    * Segments = active requirements of req type
    * Detail of children: id, name, implication count, %.
  * Change scope to: req type

## Groupings (specific)
  * Filter by grouping, incmp type, req type, implying reqs
  * Allocation graph
    * Segments = children of subject grouping
    * Detail of children: name, application count, %.
  * Change scope to: child grouping, parent grouping, top-level
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
  * Filter by grouping, incmp type, req type, implying reqs
  * Implication graph
    * Segments = reqs of subject req type
    * Detail of children: id, name, implication count, %.
  * Change scope to: top-level only
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
  * Delete
    * Usage summary (number(s) or table) of incoming references
    * Usage detail: Show each incoming references
    * List of implied reqs to also delete.
  * Filter by grouping, req type, implying reqs, text
  * Sort criteria: view, edit, reset

# Text Tokens
  * Create
  * List & View
  * Edit
  * Delete
    * Usage summary (number(s) or table)
    * Usage detail: Show each req field using it


TODO: No incmp type filter for reqs?
TODO: Text filter only for reqs?
