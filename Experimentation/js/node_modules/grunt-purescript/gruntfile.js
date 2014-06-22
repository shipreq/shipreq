/*
 * grunt-purescript
 * https://github.com/garyb/grunt-purescript
 *
 * Copyright (c) 2014 Gary Burgess
 * Licensed under the MIT license.
 */

"use strict";

module.exports = function(grunt) {

    grunt.initConfig({
    
        jshint: {
            all: [
                "gruntfile.js",
                "tasks/*.js",
                "<%= nodeunit.tests %>"
            ],
            options: {
                jshintrc: ".jshintrc",
            },
        },
        
        clean: {
            tests: ["tmp"],
        },
    
        psc: {
            simpletest: {
                files: {
                    "tmp/out.js": []
                }
            }
        },
        
        nodeunit: {
            tests: ["test/*_test.js"],
        },
    });

    grunt.loadTasks("tasks");
    grunt.loadNpmTasks("grunt-contrib-jshint");
    grunt.loadNpmTasks("grunt-contrib-clean");
    grunt.loadNpmTasks("grunt-contrib-nodeunit");

    grunt.registerTask("test", ["clean", "psc", "nodeunit"]);
    grunt.registerTask("default", ["jshint", "test"]);
};
