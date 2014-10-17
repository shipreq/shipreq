Lazy refactoring for reuse
==========================

### OO

merge code
make abstract fns for different parts
extend
impl abstract fns

final product - one big reusable thing

### FP

merge code
extract smallest parts (no deps) that are same = gives simple fns
extract parts one iota larger that are same, using args for different parts = gives HO fns
repeat until all code covered
Find sets of types often used together, create composite types
(?) Find finds with same deps, house in new class

final product - new ecosystem of fns, classes, data types

