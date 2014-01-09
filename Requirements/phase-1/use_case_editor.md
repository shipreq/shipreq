Requirements
============

UC Fields
=========
* ID
* Title
* Created By : User
* Date Created : Date
* Last Updated By : User
* Date Last Updated : Date
* Actors : Actor*
* Description : Text
* Preconditions : NestableList[TextWithTags]
* Postconditions : NestableList[TextWithTags]
* Normal Course: NestableList[TextWithTags]
* Alternative Courses: NestableList[TextWithTags]
* Exceptions: NestableList[TextWithTags]
* Use Case Relationships
* Frequency of Use
* Constraints & Business Rules
* Special Requirements
* Assumptions
* Notes & Issues

UC Composition Walkthrough
==========================
* Start fresh, date & user auto-populated.
* Enter ID & title.
* Fill out actors, precond, postcond, desc.
* Enter normal course.
  * ID.0. __________ enter title.
  *   1. ___________ enter 1.0.1
  *   2. ___________ enter 1.0.2
  *   3. ___________ press tab
  *      a. ________ enter 1.0.2.a
  *      b. ________ enter 1.0.2.b
  *      c. ________ press tab
  *   3. ___________ enter 1.0.3 and set tag "Pri:Nice"
  * Add/Remove tags
  * Link to other steps {1.0.1}
  * " -> " gets converted into unicode arrow.
  * "->1.0" gets converted into unicode arrow and link.
  * "->.3" gets converted into unicode arrow and link to step at same level.
  * Maybe have a way to copy step. "Same as 6.0.8". {see 6.0.8}
  * Insert steps, shuffles numbers, updates links.
  * Remove steps, shuffles numbers, updates links.
  * Change step level (tab), shuffles numbers, updates links.
  * Reorder step, shuffles numbers, updates links.
  * Add additional info/detail/notes/explanation/example to a step.
* Enter alternate course.
  * ID.1. __________ enter title for 1.1.
  *    ????????????? enter/select entry conditions
  *   1. ___________ enter 1.1.1
  * ID.2. __________ enter title for 1.2. Repeat.
  * Add/Remove tags to title (1.1, 1.2)
* Enter exceptions.
  * ID.E.1. __________ enter title for 1.E.1.
  * Same as alternative courses.
* Use Case Relationships.
  * Click on help for explanation of difference between include/extend
  * Include/extend/included-by/extended-by {UC-2}
  * Adds inverse link to referenced UC.
      (ie. UC-1 includes UC-2, UC-2 included by UC-1)
* Enter remaining fields.

Impl Details
============
* All fields should show examples. (?)
* Tags should be heirarchical.
  - Pri
    - Pri:Nice
    - Pri:Should
    - Pri:Must
  - WIP
    - TODO
    - TBD
    - Confirm

