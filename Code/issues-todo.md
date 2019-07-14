terminology
===========

* issue tag/type vs issue category/type

reuse (or not)
==============

* sorting
  * SortCriteriaEditor
    * SortCriteria
      * Column
      * SortMethod
      * SortCriterion
* filter
  * AST - don't modify - should be a global language with global capabilities
  * what about text-filtering against loose issue bodies?
  * FilterEditor
* column selector
  * ColumnSelector

functionality
=============

* loose issues
  * text def
  * atomscan?
  * data
  * events
  * event ap & testing
  * id ceilings (store next because hard deleting)
* UI: new
  * add loose issue editor to CreateFeature
  * add loose issue editor to EditorFeature
  * add async feature for it
* UI: table
  * table
  * dynamic columns
  * custom sorting
  * filter
  * KB navigation
* ProjectWidgets
  * conflicting tags
* Resolve TODOs in new code
