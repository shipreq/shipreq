/*
 * grunt-purescript
 * https://github.com/purescript/grunt-purescript
 *
 * Copyright (c) 2014 Gary Burgess
 * Licensed under the MIT license.
 */

"use strict";

module.exports = function (grunt) {

    var compile = function (dest, src, options, callback) {
    
        var args = src.filter(function (filepath) {
            if (!grunt.file.exists(filepath)) {
                grunt.log.warn("Source file \"" + filepath + "\" not found.");
                return false;
            } else {
                return true;
            }
        });
        
        return grunt.util.spawn({
            cmd: "docgen",
            args: args,
            options: { cwd: process.cwd() }
        }, function (err, result) {
            if (err) {
                grunt.log.error("Error creating file " + dest);
                grunt.log.error(result.stdout);
                callback(err);
            } else {
                grunt.file.write(dest, result);
                grunt.log.ok("Created file " + dest + ".");
                callback();
            }
        });

    };

    grunt.registerMultiTask("docgen", "Generate markdown documentation for PureScript modules.", function () {

        var options = this.options();
        var callback = this.async();
        var files = this.files;

        var compileNext = function (err) {
            if (err) {
                callback(err);
            } else if (files.length === 0) {
                callback();
            } else {
                var file = files.pop();
                compile(file.dest, file.src, options, compileNext);
            }
        };

        compileNext();
    });

};
