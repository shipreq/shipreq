# Setup

1. [Configure Node.JS](https://gist.github.com/japgolly/775314a0cb24e33653b059b8f8540250)
2. Install Node 11
    1. Install nvm
    2. `nvm install 11`
    3. `nvm use --delete-prefix v11.15.0`
3. `yarn install`, ctrl-c when it gets stuck on stupid semantic-UI
4. `yarn install` again and it should complete successfully.


# Files

### Input

* `shipreq` - Any custom or hand-crafted assets.
* `semantic.json5` - Semantic UI module config.
* `vendor` - Unmanaged 3rd party dependencies.
* `yarn.lock`/`package.json` - Managed 3rd party dependencies.

### Processing

* `semantic-update` - Rebuild Semantic UI.
* `build` / `build-parallel` - Builds everything else.
  * `webpack` - Bundle config.
  * `webtamp` - Asset config.

### Output

* `dist` - All output is stored here.
  * `dev` - Development-mode output. Used by `sbt` by default.
  * `prod` - Production-mode output. Used by `sbt -DMODE=release`.
  * `local` - JS bundles required by dev-side tasks and unit-tests. These are symlinked from SBT module resource dirs and referenced by filename in SBT.


# Semantic UI

Semantic UI is a special beast...

* `semantic/` and `semantic.json` *must* remain in this directory.
  Semantic UI will look for them every time npm runs and think it's not installed otherwise.

* Not all of Semantic UI (which is huge) is used. The desired subset is declared in `semantic.json5`.

* When upgrading Semantic UI, npm will change the version in `semantic.json`.
  This change must be manually applied to `semantic.json5`.

* After any change to `semantic.json5` or Semantic UI is made,
  `./semantic-update` must be run followed by `./build-parallel`
