Help
====

The goal is for users to able to learn how to use ShipReq, and use it effectively,
by themselves at their own convenience. I especially shouldn't have to give personal
demos.

Here's list of topics to organise and document.


* Project concept

* ReqTable vs ReqDetail (bulk vs single)
* Creating reqs / your first req
* unsaved changes - mechanics, yellow icon
* send feedback
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
  * transitivity
  * usage: tag-based-field, tags in text, direct input in field

* Issues
  * concept
  * manual
  * inline types, usage + suppInfo

* Implication
  * concept
  * transitivity
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


Selling Points
==============

Not sure these need their own pages.
I feel like I'm conflating help, selling points, with demo in the middle.
For now I'll at least list the thoughts...

* UX
  * FAST! Even on poor networks.
  * Close to being an offline app
  * Network activity only when a change is made.
  * Network activity non-blocking.
  * Switch back-and-forth (ReqTable↔ReqDetail) instantly, even mid-edit

* Real-time!
  * Real-time responses to nearly all operations (by avoiding network)
  * Real-time updates between users/devices/tabs (server push)

* Comprehensibility & Maintainability
  * Becomes more important as project grows
  * Implications turn a flat list of requirements into a *comprehensible* web.
  * Implications support maintainability too. (eg. delete/restore)
  * Req Codes
  * Power filter
  * Instant search feedback
  * Saved views
  * Tag distribution manager (pending)
    * See how a unit of organisation (tags / implications) is distributed amongst requirements
    * Redistribute quickly and easily to achieve balance

* Data Integrity
  * ShipReq ensures that many errors *never* occur
    * Renames not propagating
    * Non-existent data, now and forever
      * References in text (req, UC step, code)
      * Implications
      * Use case steps
    * IDs are never lost/forgotten
    * IDs are never ambiguous
  * ShipReq tracks other potential errors. It presents to user and makes resolution easy as possible.
    * data required (mandatory fields that are blank)
    * tag conflicts
    * dead refs/tags
    * empty tag/code groups
    * user-defined issues (tags & loose)
    * Issues page
      * See all outstanding issues
      * Resolve inline, no need to change pages
    * Integration
      * Dashboard
      * ReqTable
      * ReqDetail
  * Security
    * Audit history
    * Data is never discarded or lost
    * UI to explore history & audit trail (pending)
    * Save points-in-time; create baselines; mark versions (pending)

* Forgiving of Failure
  * Near-zero cost for user mistakes / experimentation / failure
  * Delete / restore anything at will
  * Reconfigure without affecting content (eg. tag/imp fields)

* High quality
  * Quality enforced code / many mechanisms employed in dev process to ensure quality (types, unit tests, prop tests, integration tests)
  * Attention to detail, edge cases
  * Thorough analysis
  * User experience
  * Capability, flexibility, genericness
