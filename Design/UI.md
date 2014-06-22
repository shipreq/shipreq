Done: 5,6,7
Pending: 12,13

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

# Incompletion types
  * Create
  * ∀incmp type.
    * Edit
    * Delete
      * Usage summary (number(s) or table)
      * Usage detail: Show each instance

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
    * Detail of children: name, application count, %.
  * Change scope to: child grouping

## Groupings (specific)
  * Filter by grouping, incmp type, req type, implying reqs
  * Allocation graph
    * Detail of children: name, application count, %.
  * Change scope to: child grouping, parent grouping, top-level
  * ∀ s : bar graph segment. list of reqs in s
    * Select/deselect req
    * Apply subject or child grouping to selected reqs.
    * Remove subject or child grouping to selected reqs.
    * Replace subject or child grouping in selected reqs, with 1 or more groupings of any kind.
  * Add child grouping
  * Edit subject or child grouping
  * Delete subject or child grouping
  * Restore child grouping

