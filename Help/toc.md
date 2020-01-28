Help
====

The goal is for users to able to learn how to use ShipReq, and use it effectively,
by themselves at their own convenience. I especially shouldn't have to give personal
demos.

Here's list of topics to organise and document.


* Project concept

* ReqTable vs ReqDetail (bulk vs single)
* Creating reqs / your first req
* Deletion
  * concept (soft, ShowDead button, restore, reason)
  * cascade
* Rich text

* ReqTypes: generic & UC
* Fields
  * custom
    * text: concept, creating, usage/effect on req{table,detail}
    * tag: concept, creating, usage/effect on req{table,detail}
    * impl: concept, creating, usage/effect on req{table,detail}
    * order
    * criteria
    * mandatory
* Tags
  * concept
  * many-to-many
  * exclusitivity
  * usage: tag-based-field, tags in text, direct input in field

* Issues
  * concept
  * manual
  * inline types, usage + suppInfo

* Implication
  * concept
  * graph in ReqDetail
  * imp graph page

* ReqTable
  * saved views
  * view customisation: column selection & order, sort
  * filter

* Use Case flow: concept, syntax, graph

* ReqCodes

* Workflows / usage recommendations
  * MF -> UC -> FR -> FR
  * filter by implicationRoot
  * Tag actors, use in text
  * Tag priority, use in text (? MCS = too few)