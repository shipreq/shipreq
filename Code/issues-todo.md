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
  * id should be editable
  * title should be editable
  * dynamic columns
  * custom sorting
* filter changes
  * issue category
  * issue class
  * issue presence
* Resolve TODOs in new code




Filter
======

* Add `issue:cat`
	* issue:badData
	* issue:futility
	* issue:missingData
	* issue:userDefined

* (DOUBT IT) add `issue:cls(:arg)`
	* issue:blankCustomField
	* issue:blankTitle
	* issue:blankUseCaseStep
	* issue:conflictingTags:"Version 1.0"
	* issue:deadIssueTag
	* issue:deadReference
	* issue:deadTag
	* issue:emptyCodeGroup
	* issue:implicationRequired:UC
	* issue:issueTag
	* issue:looseIssue
	* issue:uninhabitableTagField
