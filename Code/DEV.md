[webapp] Adding A New Data Type
===============================
* Add to .data package.
* Add to DataObjImplicits.
* Add to UnsafeTypes.
* Add to Project.
* Add to Codecs.
* Add to DataProp.
* Add to RandomData (id, data, project).
* Add to Validators.
* Add to SampleProject.
* Add to DataHash and create a new HashScheme.
* Maybe add to GenericData object used in Events.


[webapp] Events & Protocols
===========================
Project is built by applying Events.
DB contains Events.
New events can only ActiveEvents (a subset of Event).

In .protocol, RemoteFns allow client→server communication.
Most respond with VerifiedEvents.
Procedure is:

* Server and client both have π.
* Client: send α to server.
* Server creates an event: α → ε.
* Server applies event: ε → π → (π'*, Vec ε*).
* Server: sends Vec ε* to client.
* Client applies events: Vec ε* → π → (π',δ)
* Client broadcasts δ to listeners.

α  - Input to some RemoteFn.
φ  - Hash of a project.
ε  - `ActiveEvent`.
π  - `Project`.
ε* - (ε,φ). `VerifiedEvent`.
π* - (π,φ). `ServerProject.State`.
δ  - Client-side change summary. `Changes`.


[webapp-server] Security
========================

Pages are protected in `AppSiteMap`. This includes both authentication and authorisation.
Snippets do not include security logic; they simply render and perform business logic.

Available methods:
  * `Oshiro.logout`.
  * `Oshiro.loggedInUser` returns a value if a user is authenticated or remembered.
  * `Oshiro.isAuthenticated` returns a value if a user is authenticated.
  * `SnippetHelpers.currentUser_!` returns a user if authenticated or remembered, else redirects.
  * `SnippetHelpers.currentUserId_!` returns a UserId if authenticated or remembered, else redirects.
  * `PermissionCheck.userCan`.

Available snippets:
  * `Authenticated`
  * `Authenticated.not`
  * `AuthenticatedOrRemembered`
  * `AuthenticatedOrRemembered.not`
  * `LoggedInUser.email`
  * `LoggedInUser.username`

Shiro is integrated via `src/main/resources/shiro.ini` and `src/main/webapp/web.xml`.


[webapp-server] Use Case Textual Features
=========================================
* What is parsable is defined in `Grammar` with help from `ParsingConfig`.
* Parsing and translation happens in
    * `FreeText.parseCorrected()`
    * `StepText.updateCorrected()`
    * `FreeText.parseTextForFlow()`
* Anything that can be parsed in text fields, or in the main clause of step fields normally requires the following:
    * A new token in `Grammar.FreeTextToken`
    * Logic in `Grammar` to capture the new token when parsing text.
    * A new term in `FreeTextTerm`.
    * Logic in `FreeText.parseCorrected()` to validate and transform the new token into an appropriate term.
    * Make sure the FreeText laws are not satisfied: See `TextProps` in `FreeAndStepTextTests.scala`.
* Data available to parse-result translation is provided via `UcParsingCtx`.
* To change text in reaction to other changes
    * Add a case to `respondToChange()` in `FreeText`/`StepText`.
    * Perform the reaction in `TextProps.arbState` in `FreeAndStepTextTests.scala`.

[webapp-server] Snippet Testing
===============================
* TestHelpers
  * inMockSession(...)
  * withSessionAttrs    - S.attr
  * withSessionParams   - S.param
  * withUserLoggedIn    - Data: UD1, UD2
  * assertJsAlert
  * assertJsErrorNotice
  * assertRedirect      - Catches redirect-exception
* MockDaoProvider{cfg}.install{test}
* CssTestHelpers
  * findCss
* unquoteJs/js2str in UseCaseEditorTest
