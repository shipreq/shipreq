# grunt-purescript

> Runs the [PureScript](https://github.com/purescript/purescript) compiler to produce JavaScript files.

## Getting started
This plugin requires Grunt `~0.4.2` and [PureScript](http://hackage.haskell.org/package/purescript) `>=0.4.14`

If you haven't used [Grunt](http://gruntjs.com/) before, be sure to check out the [Getting Started](http://gruntjs.com/getting-started) guide, as it explains how to create a [Gruntfile](http://gruntjs.com/sample-gruntfile) as well as install and use Grunt plugins. Once you're familiar with that process, you may install this plugin with this command:

```shell
npm install grunt-purescript --save-dev
```

Once the plugin has been installed, it may be enabled inside your Gruntfile with this line of JavaScript:

```js
grunt.loadNpmTasks('grunt-purescript');
```

## The "psc" task

### Overview
In your project's Gruntfile, add a section named `psc` to the data object passed into `grunt.initConfig()`.

```js
grunt.initConfig({
  psc: {
    options: {
      // Task-specific options go here.
    },
    your_target: {
      // Target-specific file lists and/or options go here.
    },
  },
});
```

### Options

#### options.main
Type: `Boolean` or `String`
Default value: `false`

Toggles the `--main` compiler flag. Can be set to `true` or the name of a module in which a `main` function resides. When enabled, a call to `main` will be added after all other generated JavaScript. When set to `true`, the module name will be assumed to be `Main`.

#### options.modules
Type: `String` or `Array`
Default value: none

Enables dead code elimination, ensuring that the named module (or list of modules) are included in the generated JavaScript, along with all their dependencies.

#### options.codegen
Type: `String` or `Array`
Default value: none

Specifies which module(s) to include in the generated Javascript and externs files.

#### options.externs
Type: `String`
Default value: none

Invokes the `--externs` compiler flag with the specified argument. Generates a `.externs` file for foreign imports.

#### options.browserNamespace
Type: `String`
Default value: `PS`

Invokes the `--browser-namespace` compiler flag with the specified argument. Specifies the namespace that PureScript modules will be exported to when running in the browser.

#### options.noPrelude
Type: `Boolean`
Default value: `false`

Toggles the `--no-prelude` compiler flag. Omits the Prelude from the generated JavaScript when enabled.

#### options.noOpts
Type: `Boolean`
Default value: `false`

Toggles the `--no-opts` compiler flag. Skips the optimization phase for the generated JavaScript when enabled.

#### options.noMagicDo
Type: `Boolean`
Default value: `false`

Toggles the `--no-magic-do` compiler flag. Disables overloading of the `do` keyword to inline calls to `>>=` for the `Eff` monad to generate more efficient code.

#### options.noTco
Type: `Boolean`
Default value: `false`

Toggles the `--no-tco` compiler flag. Disables tail-call elimination on the generated JavaScript.

#### options.runtimeTypeChecks
Type: `Boolean`
Default value: `false`

Toggles the `--runtime-type-checks` compiler flag. Generates simple runtime type checks for function arguments with simple types when enabled.

## The "pscMake" task

### Overview
This task runs the `psc-make` executable, which will compile modules to their own `.js` and `.externs` files. If no `dest` is specified the files will be generated in the `output/` folder. This mode is useful when developing large libraries, since it avoids recompiling unchanged modules.

In your project's Gruntfile, add a section named `pscMake` to the data object passed into `grunt.initConfig()`.

Basic usage, generating the files in `output/`:

```js
grunt.initConfig({
  pscMake: ["path/to/source/**/*.purs"]
});
```

With options:

```js
grunt.initConfig({
  pscMake: {
    options: {
      // Task-specific options go here.
    },
    src: ["path/to/source/**/*.purs"]
  },
});
```

Or to specify an output folder a named target must be used (`lib` in this case):

```js
grunt.initConfig({
  pscMake: {
    lib: {
      src: ["path/to/source/**/*.purs"],
      dest: "build"
    }
  },
});
```

### Options

- options.browserNamespace
- options.noPrelude
- options.noOpts
- options.noMagicDo
- options.noTco
- options.runtimeTypeChecks

These options have the same affect as described for the `psc` task above.

## The "dotPsci" task

### Overview
This task generates or updates a `.psci` file in the current directory, adding `:m` commands for any files matching a list of source patterns.

```js
grunt.initConfig({
  dotPsci: ["path/to/source/**/*.purs"]
});
```

### Options

There are no options for `dotPsci`.