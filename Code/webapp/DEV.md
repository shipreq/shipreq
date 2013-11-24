Javascript
==========

Javascript is stored in both `src/main/javascript` and `src/main/webapp/js`, the difference being that the former gets
run through Google Closure and the latter is used as-is.

When `sbt compile` or `sbt js` is run, Google Closure is used to transform the JS in `src/main/javascript`.
It aggregates via directives `// require "xxx.js"`, and minimises.
The results are saved to `target/scala-2.10/resource_managed/main/js/`

From the webapp, all Javascript lives in `/js`.
Examples:
    `src/main/javascript/project.js`      --> `/js/project.js'
    `src/main/webapp/js/vendor/jquery.js` --> `/js/vendor/jquery.js'


Security
========

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


Use Case Textual Features
=========================
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

Snippet Testing
===============
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
