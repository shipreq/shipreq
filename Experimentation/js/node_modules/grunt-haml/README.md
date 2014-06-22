# grunt-haml [![Build Status](https://secure.travis-ci.org/concordusapps/grunt-haml.png?branch=master)](http://travis-ci.org/concordusapps/grunt-haml)

> Process HAML templates to precompiled JavaScript or rendered HTML.


## Getting Started
If you haven't used [grunt][] before, be sure to check out the [Getting Started][] guide, as it explains how to create a [gruntfile][Getting Started] as well as install and use grunt plugins. Once you're familiar with that process, install this plugin with this command:

```shell
npm install grunt-haml --save-dev
```

[grunt]: http://gruntjs.com/
[Getting Started]: http://gruntjs.com/getting-started


## Haml task
_Run this task with the `grunt haml` command._

_This task is a [multi task][] so any targets, files and options should be specified according to the [multi task][] documentation._
[multi task]: https://github.com/gruntjs/grunt/wiki/Configuring-tasks


### Options

#### language
Type: ```string```
Default: ```js```

Specifies the script language and compiler to use alongside HAML.

Accepts following values: ```coffee```, ```js```, or ```ruby```.
If given ```coffee``` or ```js``` it will use
[haml-coffee][] or [haml-js][] respectively. If given ```ruby``` it will
shell out to the [haml gem] executable.

[haml-coffee]: https://github.com/netzpirat/haml-coffee
[haml-js]: https://github.com/creationix/haml-js
[haml gem]: http://rubygems.org/gems/haml

#### target
Type: ```string```
Default: ```html```

Specifies the target language to compile to.

Accepts the following values: ```js``` or ```html```. For ```js``` the template
is generated and for ```html``` the template is both generated and rendered
into its resultant HTML.

If ```language``` is set to ```ruby``` then ```target```
must be set to ```html```.

#### placement
Type: ```string```
Default: ```global```

Specifies where to place the resultant template.

Currently accepts either ```global``` or ```amd```.
- ```global``` places the template on the window.
- ```amd``` uses AMD to load the template.

*Defined only for target == 'js'.*

#### namespace
Type: ```string```
Default: ```window.HAML```

Specifies the global object to store the compiled HAML templates in.

*Defined only for target == 'js' and placement == 'global'.*

#### name
Type: ```string```
Default: basename without extension of the input HAML file

Specifies the name to store the compiled HAML template as in the object
specified by `namespace`.

With a `name` of 'apple' and a namespace of `this._template`, you'd get
something like this:

```javascript
window.HAML['apple'] =  function(locals) {
    // template code
};
```

*Defined only for target == 'js' and placement == 'global'.*


#### context
Type: ```object```
Default: ```{}```

Set variables that can be evaluated within haml templates.

The haml file:

    %h1= "hello #{ @greet }"

With the configuration

    context: {
      'greet': 'Morgan Freeman'
    }

Would compile to

    <h1>hello Morgan Freeman</h1>

*Defined only for target == 'html'*


#### dependencies
Type: ```object```
Default: ```{}```

Specifies any module dependencies of the HAML file that must be loaded via
the AMD define statement. These are automatically appended to if there are
any `require` statements in the HAML code.

With the following HAML file:

```haml
!= require('path/to/other')()
%p Hello World
```

And the following options (in the `Gruntfile.js`):

```javascript
haml: {
  compile: {
    files: // ...
    options: {
      dependencies: {
        $: 'jquery',
        _: 'underscore'
      }
    }
  }
}
```

The resultant template would be something like this:

```javascript
define(['jquery', 'underscore', 'path/to/other'], function($, _, other) {
    // template code
});
```

*Defined only for target == 'js'.*

#### bare
Type: ```boolean```
Default: ```true```

Compile the JavaScript without the top-level function safety wrapper.

*Defined only for language == 'coffee' and target == 'js'.*

#### precompile
Type: ```boolean```
Default: ```true```

Process HAML templates to precompiled JavaScript or not. Defaults to `true` for
`target == 'js'` and is forced to `false` for `target == 'html'`.

#### rubyHamlCommand
Type: ```string```
Default: ```haml -t ugly```

The shell command which will be ran to compile the HAML. The path to the
HAML file will be passed as the last command-line argument.

*Defined only for language == 'ruby'

#### includePath
Type: ```boolean```
Default: ```false```

Specifies whether or not to include the relative path in automatic generated
name.

When enabled, you'll get results like window.HAML['path/to/template']
instead of window.HAML['template'].

*Defined only `placement == 'global'`.*

#### pathRelativeTo
Type: ```string```
Default: ```./```

Specifies the path names will be based from.

If `pathRelativeTo == ./templates/` you would get:

```
window.HAML['example']
```

Otherwise, with `./` you'll get:

```
window.HAML['templates/example']
```

*Defined only `placement == 'global'`.*

### Usage examples

``` javascript
haml: {
  one: {
    files: {
      // 1:1 compile
      'path/to/result.js': 'path/to/source.haml',

      // compile and concat into single file
      'path/to/another.js': ['path/to/sources/*.haml', 'path/to/more/*.haml']
    }
  },

  // compile individually into dest, maintaining folder structure
  two: {
    files: grunt.file.expandMapping(['path/to/*.haml'], 'path/to/dest/', {
      rename: function(base, path) {
        return base + path.replace(/\.haml$/, '.js');
      }
    })
  }
}
```


## Release History

 * 2013-10-09   v0.8.0   Update version of haml-coffee 1.13.x Rendering to HTML with partials now works correctly
 * 2013-08-21   v0.7.0   Replace execSync with proper use of exec.
 * 2013-08-11   v0.6.1   Update version of haml-coffee 1.11.x
 * 2013-08-06   v0.6.0   Add 'precompile', 'includePath', and 'pathRelativeTo' option @leahciMic
 * 2013-05-27   v0.5.0   Add 'ruby' as a language option Update version of haml-coffee 1.10.x
 * 2013-04-15   v0.4.0   Full support of grunt 0.4.x
 * 2013-03-21   v0.3.2   Corrected placement option to match readme.
 * 2013-01-27   v0.3.1   Update to current version of grunt.
 * 2012-12-19   v0.3.0   Name changed to `grunt-haml` Default target changed to `html`
 * 2012-12-18   v0.1.3   Updated README.
 * 2012-12-18   v0.1.2   Fixed package.json syntax error.
 * 2012-12-18   v0.1.1   Fixed package.json dependencies.
 * 2012-12-18   v0.1.0   Rewrite from scratch (referencing grunt-contrib). Support for full gamut of options.
 * 2012-12-16   v0.0.2   Fixed a syntax error in the package.json (missing comma).
 * 2012-11-06   v0.0.1   Initial release; only slightly modified from what `grunt-contrib-coffee` was at the time.

---

Task submitted by [Ryan Leckey](https://github.com/mehcode)

*This file was generated on Wed Jan 29 2014 11:31:48.*
