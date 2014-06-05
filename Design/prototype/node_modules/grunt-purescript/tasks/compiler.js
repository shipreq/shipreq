/*
 * grunt-purescript
 * https://github.com/purescript/grunt-purescript
 *
 * Copyright (c) 2014 Gary Burgess
 * Licensed under the MIT license.
 */

"use strict";

module.exports = function (grunt) {

    var flagOptions = {
        noPrelude: "--no-prelude",
        noOpts: "--no-opts",
        noMagicDo: "--no-magic-do",
        noTco: "--no-tco",
        runtimeTypeChecks: "--runtime-type-checks"
    };

    var argumentOptions = {
        browserNamespace: "--browser-namespace",
        externs: "--externs"
    };
    
    var moduleOptions = {
        modules: "module",
        codegen: "codegen"
    };

    var compile = function (dest, src, options, callback) {
    
        // Get source file and common command line arguments
        var args = getDefaultArgs(src, options);
        
        var addModuleOption = function (mo) {
            var option = options[mo];
            if (option) {
                var optionArg = moduleOptions[mo];
                if (typeof option === "string") {
                    args.push("--" + optionArg + "=" + option);
                } else {
                    option.forEach(function (module) {
                        args.push("--" + optionArg + "=" + module);
                    });
                }
            }
        };

        // Add arguments that can accept a list of module names
        for (var mo in moduleOptions) {
            if (moduleOptions.hasOwnProperty(mo)) {
                addModuleOption(mo);
            }
        }

        // Add the main module argument
        if (options.main) {
            if (options.main === true) {
                args.push("--main");
            } else {
                args.push("--main=" + options.main);
            }
        }

        // Add the destination file output argument
        args.push("--output=" + dest);

        // Run the compiler
        return grunt.util.spawn({
            cmd: "psc",
            args: args,
            options: { cwd: process.cwd() }
        }, function (err, result) {
            if (err) {
                grunt.log.error("Error creating file " + dest);
                grunt.log.error(result.stdout);
                callback(err);
            } else {
                grunt.log.ok("Created file " + dest + ".");
                callback();
            }
        });

    };

    var getDefaultArgs = function(src, options) {

        // Use the input file list as the initial arguments
        var args = src.filter(function (filepath) {
            if (!grunt.file.exists(filepath)) {
                grunt.log.warn("Source file \"" + filepath + "\" not found.");
                return false;
            } else {
                return true;
            }
        });

        // Add any option flags
        for (var flag in flagOptions) {
            if (flagOptions.hasOwnProperty(flag)) {
                if (options[flag] === true) {
                    args.push(flagOptions[flag]);
                }
            }
        }

        // Add any option arguments
        for (var arg in argumentOptions) {
            if (argumentOptions.hasOwnProperty(arg)) {
                if (typeof options[arg] === "string") {
                    args.push(argumentOptions[arg] + "=" + options[arg]);
                }
            }
        }

        return args;
    };

    grunt.registerMultiTask("psc", "Compile PureScript files.", function () {

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

    grunt.registerMultiTask("pscMake", "Compile PureScript files in make mode.", function () {
        
        var callback = this.async();
        
        // Get source file and common command line arguments
        var args = getDefaultArgs(this.filesSrc, this.options());
        
        if (this.data.dest) {
            args.push("--output=" + this.data.dest);
        }
        
        // Run the compiler
        return grunt.util.spawn({
            cmd: "psc-make",
            args: args,
            options: { cwd: process.cwd() }
        }, function (err, result) {
            if (err) {
                grunt.log.error("Make failed:");
                grunt.log.error(result.stdout);
                callback(err);
            } else {
                grunt.log.ok("Make was successful.");
                callback();
            }
        });
    });

};
