# Usage examples

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
