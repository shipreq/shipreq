/*
 * grunt-purescript
 * https://github.com/purescript/grunt-purescript
 *
 * Copyright (c) 2014 Gary Burgess
 * Licensed under the MIT license.
 */

"use strict";

var compilerTasks = require("./compiler");
var psciTask = require("./psci");
var docgenTask = require("./docgen");

module.exports = function (grunt) {

    compilerTasks(grunt);
    psciTask(grunt);
    docgenTask(grunt);

};
