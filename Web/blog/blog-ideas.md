* formatting dates in Gatsby using the user's locale
  (formatDate in GraphQL has a hardcoded default locale of `en` in Gatsby)

* scalajs-react SSR

* how i ensure quality
  * maybe include a philosophy/personal view section?
    * internal goals
    * quality means dev scalability, happy devs, less bug fixing, confidence my stuff works
    * when I'm implementing a new hard/complex feature, all the tools are in place ready for me. testing = easy
    * quality means happy users
    * quality means faster iteration
    * quality makes me happy
  * unit tests
  * prop tests
  * integration tests (test-state, user PoV/UX)
  * TLA+
  * types - cover a lot, can't cover all
  * DataProp
  * protocols/codecs
  * random gen of valid entire projects
  * random gen of valid event streams
  * feature abstraction level
     - hardcoding: not very abstraction, less capable, more surface area when combining features, scales medium
     - flexible, highly config: very abstract, super capable, huge surface area, typically near impossible to scale
     - i'm not shitting on tools that are very limited. I'm not shitting on JIRA.
       the reason i've gone super flexible is because that's what real teams need when you look at the big picture.
       most tools only look at a small subset, and go limited = they provided a limited solution to a subset of your problem

* Docker cache-from stupidity ( d595f2047a3491e6574bc696c12d6aa0ae19d9c2 )

* terraform: my ec2-{sd,ebs} modules?
