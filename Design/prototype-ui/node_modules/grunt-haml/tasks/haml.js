/*
 * grunt-contrib-haml
 *
 * Copyright (c) 2012 Concordus Applications
 * Licensed under the MIT license.
 */

module.exports = function(grunt) {
  'use strict';

  var path = require('path');
  var async = grunt.util.async;
  var _    = grunt.util._;

  grunt.registerMultiTask('haml', 'Compile Haml files', function() {
    // Set the target options with some defaults.
    var options = this.options({
      // Default target is javascript.
      target: 'html',

      // Default language choice to coffee-script and haml-coffee.
      language: 'js',

      // Default placement; either `amd` or `global`.
      placement: 'global',

      // Default global placement namespace to `window`.
      namespace: 'window.HAML',

      // Default hash of dependencies for AMD.
      dependencies: {},

      // Default should name include path
      includePath: false,

      // Default path relative to
      pathRelativeTo: './',

      // External haml command to execute, must accept STDIN
      rubyHamlCommand: 'haml -t ugly',

      // Precompile templates; if false (and target == 'js'), place rendered
      // HTML in js variables.
      precompile: true

    });

    // Make the test async.
    var done = this.async();

    // Write options iff verbose.
    grunt.verbose.writeflags(options, 'Options');

    // Transpile each src/dest group of files.
    async.forEach(this.files, function(file, callback) {
      var opts;
      // Get only files that are actually there.
      var validFiles = file.src.filter(function(filepath) {
        if (grunt.file.exists(filepath)) {
          return true;
        } else {
          grunt.log.warn('Source file "' + filepath + '" not found.');
          return false;
        }
      });

      // Ensure we have files to transpile.
      if (validFiles.length > 0) {

        var processFile = function(filename, cb) {
          opts = _.extend({}, options);

          // Ensure we have a template name.
          if (opts.name === undefined) {
            // Default template name is the basename w/o the extension.
            // or relative path to + basename w/o extension if
            // includePath is true
            var defaultPath = '';
            if (opts.includePath) {
              defaultPath = path.relative(
                opts.pathRelativeTo, path.dirname(filename));

              if (defaultPath !== '') {
                defaultPath += '/';
              }
            }
            opts.name = defaultPath + path.basename(
              filename, path.extname(filename));
          }

          transpile(filename, opts, function(out) { cb(null, out); });
        };

        // Transpile each file.
        async.map(validFiles, processFile, function (err, results) {
          // Write the new file.
          grunt.file.write(file.dest, results.join('\n'));
          grunt.log.writeln('File ' + file.dest.cyan + ' created.');

          // Let the series know were done.
          callback();
        });
      } else {
        grunt.log.writeln('Unable to compile; no valid files were found.');
      }
    }, done);
  });

  var transpile = function(name, opts, cb) {
    // Construct appropriate options to pass to the compiler
    // Create a new object so we do not mutate the target options.
    var options = _.extend({filename: name}, opts);

    // Read in the file
    options.input = grunt.file.read(name);

    try {
      switch (options.language) {
      case 'js': transpileJs(options, cb); break;
      case 'coffee': transpileCoffee(options, cb); break;
      case 'ruby': transpileRuby(options, cb); break;
      default:
        grunt.fail.warn(
          'Language ' + options.language + ' is not a valid ' +
          'source language for HAML; choices are: coffee and js\n');
      }
    } catch (e) {
      grunt.log.error(e);
      grunt.fail.warn('Haml failed to compile.');
    }
  };

  var transpileJs = function(options, cb) {
    var haml = require('haml');

    // First pass; generate the javascript method.
    var output = haml(options.input);

    if (options.target === 'html') {
      // Evaluate method with the context and return it.
      cb(output(options.context));
      return;
    } else if (options.target !== 'js') {
      grunt.fail.warn(
        'Target ' + options.target + ' is not a valid ' +
        'destination target for `haml-js`; choices ' +
        'are: html and js\n');
    }

    if (options.precompile) {
      // Reduce to a true annoymous, unnamed method.
      // TODO: Push this upstream.
      output = output.toString().substring(27);
      output = 'function(locals)' + output;
    } else {
      output = htmlescape(output(options.context));
    }

    // Return the final template.
    cb(wrapIt(output, options));
  };

  var htmlescape = function(input) {
    input = input.replace('</script>', '</scr" + "ipt>');
    input = JSON.stringify(input) + ';';
    return input;
  };

  var wrapIt = function(input, options) {
    switch (options.placement) {
      case 'global':
        // Set in the desired namespace.
        input = options.namespace + "['" + options.name + "'] = " + input;
        input = '\n' + input + '\n';
        break;

      case 'amd':
        // Search for additional dependencies
        var lookup = /require.*?\(.*?["'](.*)["'].*?\)/g;
        var extra = lookup.exec(options.input);
        while (extra !== null) {
          var base = path.basename(extra[1]);
          options.dependencies[base] = extra[1];
          extra = lookup.exec(options.input);
        }

        // Build define statement.
        var defineStatement = 'define([';
        var modules = _.values(options.dependencies);
        modules = modules.length ? "'" + modules.join("','") + "'" : "";
        defineStatement += modules;
        defineStatement += '], function(';
        defineStatement += _.keys(options.dependencies).join(',');
        defineStatement += ') { \n';

        // Wrap input in it.
        input = defineStatement + 'return ' + input + ';\n});\n';
        break;

      default:
        grunt.fail.warn(
          'Placement ' + options.placement + ' is not a valid ' +
          'destination placement for `haml-js`; choices ' +
          'are: amd and global\n');
    }
    return input;
  };

  var transpileCoffee = function(options, cb) {
    var hamlc = require('haml-coffee');

    switch (options.target) {
      case 'js':
        if (options.precompile) {
          // Pass it off to haml-coffee to render a template in javascript.
          cb(hamlc.template(options.input, options.name, options.namespace,
            options));
        } else {
          // Pass it off to haml-coffee to render a template in javascript.
          var output = hamlc.compile(options.input, options)(options.context);
          cb(wrapIt(htmlescape(output), options));
        }

        break;

      case 'html':
        // Pass it off to haml-coffee to render a template in javascript.
        cb(hamlc.render(options.input, options.context, options));
        break;

      default:
        grunt.fail.warn(
          'Target ' + options.target + ' is not a valid ' +
          'destination target for `haml-coffee`; choices ' +
          'are: html and js\n');
      }
  };

  var transpileRuby = function(options, callback) {
    var exec = require('child_process').exec,
        output;

    if (options.context) {
      grunt.fail.warn("Context is not a valid option for `haml-ruby`");
    }

    if (options.target !== 'html') {
      grunt.fail.warn(
        'Target ' + options.target + ' is not a valid ' +
        'destination target for `haml-ruby`; choices ' +
        'are: html\n');
    }

    var p = path.resolve(options.filename);
    var command = options.rubyHamlCommand + ' ' + p;
    var result = exec(command, function(error, stdout, stderr) {
      if (result.error || error) {
        grunt.fail.warn(
          "Error executing haml on " + p + ": \n" +
          stderr + "\n" +
          stdout
        );
      }

      if (stderr) {
        grunt.log.warn(
          "Error executing haml on " + p + ": \n" +
          stderr + "\n" +
          stdout
        );
      }

      output = stdout;

      if (options.wrapHtmlInJs) {
        output = wrapIt(htmlescape(output), options);
      }

      callback(output);
    });
  };
};
