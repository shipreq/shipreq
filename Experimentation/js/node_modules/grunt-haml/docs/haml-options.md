# Options

## language
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

## target
Type: ```string```
Default: ```html```

Specifies the target language to compile to.

Accepts the following values: ```js``` or ```html```. For ```js``` the template
is generated and for ```html``` the template is both generated and rendered
into its resultant HTML.

If ```language``` is set to ```ruby``` then ```target```
must be set to ```html```.

## placement
Type: ```string```
Default: ```global```

Specifies where to place the resultant template.

Currently accepts either ```global``` or ```amd```.
- ```global``` places the template on the window.
- ```amd``` uses AMD to load the template.

*Defined only for target == 'js'.*

## namespace
Type: ```string```
Default: ```window.HAML```

Specifies the global object to store the compiled HAML templates in.

*Defined only for target == 'js' and placement == 'global'.*

## name
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

## dependencies
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

## bare
Type: ```boolean```
Default: ```true```

Compile the JavaScript without the top-level function safety wrapper.

*Defined only for language == 'coffee' and target == 'js'.*

## precompile
Type: ```boolean```
Default: ```true```

Process HAML templates to precompiled JavaScript or not. Defaults to `true` for
`target == 'js'` and is forced to `false` for `target == 'html'`.

## rubyHamlCommand
Type: ```string```
Default: ```haml -t ugly```

The shell command which will be ran to compile the HAML. The path to the
HAML file will be passed as the last command-line argument.

*Defined only for language == 'ruby'

## includePath
Type: ```boolean```
Default: ```false```

Specifies whether or not to include the relative path in automatic generated
name.

When enabled, you'll get results like window.HAML['path/to/template']
instead of window.HAML['template'].

*Defined only `placement == 'global'`.*

## pathRelativeTo
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
