# ShipReq

This is a project called ShipReq.
It is a platform for storing requirements and related data.

It consists of two servers:
  1. webapp - serves a webapp that users interact with.
  2. taskman - a backend that receives messages on a queue in the database and performs background tasks.

## Webapp

There are three SPAs, all built using scalajs-react:
  1. webapp-client-public - what the user sees when they're not logged in
  2. webapp-client-home - what the user sees when they first log in; from here they can create projects
  3. webapp-client-project - the main app of the system, this allows users to view and edit a project (which is where all their requirements live)

webapp-client-ww is a web-worker used by webapp-client-project.

The frontend assets are generated in the `frontend` directory.
This uses React and Semantic UI.

# Instructions

* Code should be as strongly-typed as possible so that the compiler (both now and in the future) maintains the correctness of code as much as possible
* Always prefer immutable data over mutable data
* Code should be tested thoroughly
* Never use `==` or `!=`; instead use `==*` or `!=*` from the `univeq` library
* Implicit instances of `UnivEq` should always be `def`s and not `val`s

* When you grep, ignore the .metals directory

* When creating UI:
  * don't create stateful React components; make them stateless and pass in StateSnapshot through the props.

* When creating UI TestState-based tests:
  * all observations should go into an Obs class
  * all instances of `*.focus` or `*.action` should go in a TestDsl class
  * Don't use `ProjectSpaTestDsl.*`, only use the `*` DSL created in each test suite's `TestDsl`
  * Use CommonObs where appropriate (eg. use it to observe inputs, textareas, dropdowns, etc)
  * If a test suite requires global references (eg. an instance of `TestConfirmJs`) request them using a Ref class (which is the first argument to the TestState `DSL` when creating `*` vals)
  * Compose TestState actions using the >> operator
  * Compose TestState assertions using the +> operator
