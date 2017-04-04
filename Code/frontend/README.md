# Setup

1. [Configure Node.JS](https://gist.github.com/japgolly/775314a0cb24e33653b059b8f8540250)
2. `yarn install`, ctrl-c when it gets stuck on stupid semantic-UI
3. `yarn install` again and it should complete successfully.

# Files

### Input

* `shipreq` - Any custom or hand-crafted assets.
* `semantic-ui/semantic.json5` - Semantic UI module config.
* `vendor` - Unmanaged 3rd party dependencies.
* `yarn.lock`/`package.json` - Managed 3rd party dependencies.

### Processing

* `semantic-ui/update` - Rebuild Semantic UI.
* `build` / `build-parallel` - Builds everything else.
  * `webpack` - Bundle config.
  * `webtamp` - Asset config.

### Output

* `dist` - All output is stored here.
  * `dev` - Development-mode output. Used by `sbt` by default.
  * `prod` - Production-mode output. Used by `sbt -DMODE=release`.
  * `local` - JS bundles required by dev-side tasks and unit-tests. These are symlinked from SBT module resource dirs and referenced by filename in SBT.
