title: ShipReq
layout: true

<!--
.bottom-bar[
  {{title}}
]

NOTE TO SELF: To prepare demo,
* Build Docker images in release mode
* `bin/env dev up`
* https://local.shipreq.com
* Ensure user exists and can login
* Create req-code demo
-->

---
class: middle, center

![x](logo-title.svg)

PRODUCT SHOWCASE

---

# ShipReq is...
a real-time, cloud-based web platform for businesses to

* record
* maintain
* socialise

requirements.

---
name: requirements

# Requirements

Requirements are details that describe and communicate the
<br>who, what, when, where, why, how of a goal.
<br><br><br>

---
template: requirements

#### Computing:
* software development
* hardware development
* graphic design
* etc.

---
template: requirements

#### Manufacturing:
* cars
* furniture
* pace makers
* kettles
* etc.

---
template: requirements

#### Social alignment:
* governance
* contracts
* legal
* etc.

---

# Requirements Matter

Some stats from studies:
--

* Portland Business Journal: “Most analyses conclude that between 65 and 80% of IT projects fail to meet their
  objectives, and also run significantly late or cost far more than planned.”
--

* 2011: 78% of organizations reported that the “Business is usually or always out of sync with project requirements”
--

* 2010: 70% of organizations have suffered at least one project failure in the prior 12 months
--

* 2008: 60% of projects do not meet schedule, budget and quality goals
--

* 17% of IT projects run so badly that they threaten the very existence of a company

---

# Requirements Matter

Some stats from studies:

* It costs 10-70x more to fix a problem during the dev & testing phases, than during the requirements phase

--

* It costs 40-1000x more to fix a problem when an app is live, than during the requirements phase
--

* Rework often consumes 30-50% of total dev cost
--

* 70-85% of rework is caused by bad requirements

---

# Initial focus

ShipReq's initial focus is software development,<br>
my home and my strength.

---

# Current Industry State

--

* office, MS word & Excel, Google docs
--

* issue trackers, Jira
--

* post-it notes, Trello
--

* vendor product
--

* mental

---

# Problems

--

* ease of access
--

* ease of use
--

  * wrong solution fit
--

  * flexibility
--

  * UX
--

* data integrity
--

* doesn't scale
--

  * too many requirements = hard to comprehend
--

  * too many requirements = hard to keep accurate
--

  * too many requirements = hard to socialise
--

  * too many requirements = loss of faith, resort to offline

---

# Problems

* social
--

  * miscommunication
  * information socialisation (esp wrt change)
  * lack of transparency
  * lack of accountability
  * The Blame Game

---

# Enter: ShipReq

<br>I very passionately want to solve these problems.

--

<br>I'm a software developer; I want to use this!

--

<br>Software is ubiquitous; as a consumer I'm sick of buggy shit.

--

<br>I want to help improve the industry.

---

# Roadmap

I'm not there yet. I'm just getting started...

--

* v1.0: Use Cases

---

TODO Add a screenshot of a use case

---

TODO Screenshot of a use case in ShipReq

---

# Roadmap

* v1.0: Use Cases

  Initial feedback:
  * very positive
  * need to handle other requirement types
  * a few feature requests

---

# Roadmap

* v1.0: Use Cases
* v2.0: General Requirements
--

  * MVP in terms of business scope
--

  * quite featureful within scope, not a toy
--

  * *we're here now*

---

# Roadmap

* v1.0: Use Cases
* v2.0: General Requirements *[Sep 2017]*
* Find co-founder; business-mode: on *[Oct 2017]*
--

* v3.0: Collaboration, enterprise, social
--

* Future versions: many, many ideas
  <br>Will triage later according to market and customer feedback

---

# Features

(and product demo)

*new project → req table → create a few reqs*

---
exclude: true

====================================================================================================

* Defining attributes
  * Quality
  * Attn to detail
  * Ease of use
  * Capability

*new project → req table → create a few reqs*

* Requirement Types
  * Use Cases
  * User-defined
* Switch to cfg/reqtypes & back (no network)

* Rich text
  * Refs (show hover)
  * Lists
  * Links (web/mail)
  * Math
  * More interesting features later

* UX
  * ReqTable: Excel-like, bulk mindset, edit many in parallel
  * ReqDetail: single focus, detailed mindset

* UX
  * FAST! Even on poor networks.
  * Only network activity when a change is made. Non-blocking.
  * Switch back-and-forth (Table↔Detail), even mid-edit
* ReqDetail → CfgFields → change order, mod text field → ReqDetail

* UX
  * Real-time
    * updates between users/devices/tabs
    * responses to nearly all operations

* Tags
  * in Tags column
  * in text
  * in own columns (many:many & transitive)

* Implications
  A → B
  If requirement A exists, requirement B must exist too.
  If requirement A doesn't exist, requirement B might not need to exist.
  Turns a flat list of requirements into an explicable web

* Demonstration

* New Use Case
  * Create flow
  * Show diagram

* New tab, create GRs implied by the UC

* Create a GR implied by a GR
* Show locality graph in ReqDetail
* Show project ImpGraph

* Create a MF and imply UC
* Show transitivity: MF → UC → FRs
* Create imp column

* Comprehensibility & maintainability
  * Becomes more important as project grows
  * Implications aid comprehensibility
  * Delete UC: Show implied reqs auto-selected

* Comprehensibility
  * Another tool: req codes
    * open demo project, show with/without
    * show reappearance for cross-concerns

* Comprehensibility
  * Power filter
  * Instant search feedback

* Comprehensibility
  * Distribution manager (planned)

# Data Integrity
Huge problem as requirements projects grow.
1. Preventable errors.
1. Detectable errors.
1. Security.

* Preventable errors
  * Links between requirements
    * References in text (req, UC step, code)
    * Implications
    * Use case flow

* Preventable errors
  * IDs never lost (change req type, reqtype mnemonic)

* Detectable errors
  * mandatory fields
  * tag conflicts
  * dead refs
  * all implying reqs are dead
  * user-defined issues
  * loose issues

* Detectable errors
  * screen WIP

* Security
  * Audit history
  * Tamper-proof (similar to crypto-currencies like Bitcoin)
  * UI pending

* Data Integrity & UX (ease-of-use + feedback speed) mean:
  * Cost of user failure/play/experimentation nearly zero
    * Delete/undelete anything at will
    * Tag/imp fields are dynamic views, detached from storage

====================================================================================================


Alternative Businesses
- Expand into other domains (contracts, governance, medical, etc.)
- BA consultancies
  - Licence to
  - Parter with
  - Create own

3.0 - Social
* no more confusion between teams, external parties, individuals
* no more blame games when things go wrong
* automation of information socialisation
* accountability

whatItIs
domainSummary
domainFamily
