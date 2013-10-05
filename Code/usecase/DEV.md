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

