/*
 * grunt-purescript
 * https://github.com/purescript/grunt-purescript
 *
 * Copyright (c) 2014 Gary Burgess
 * Licensed under the MIT license.
 */

"use strict";

module.exports = function (grunt) {

    grunt.registerMultiTask("dotPsci", "Generate/update .psci file to load a set of modules on startup.", function () {
        
        var entries = [];
        var isNew = true;
        
        try {
            entries = grunt.file.read(".psci").split("\n");
            isNew = false;
        } catch (e) {
            if (e.origError.code !== "ENOENT") {
                grunt.log.error(e);
                return;
            }
        }
        
        entries = entries.filter(function (entry) {
            return entry.indexOf(":m") !== 0;
        });
        
        entries = this.filesSrc.map(function (file) {
            return ":m " + file;
        }).concat(entries);
        
        grunt.file.write(".psci", entries.join("\n"), "utf8");
        grunt.log.ok((isNew ? "Created" : "Updated") + " .psci file");
    });

};
