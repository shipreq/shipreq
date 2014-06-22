/*
 * grunt-contrib-haml
 *
 * Copyright (c) 2012 Concordus Applications
 * Licensed under the MIT license.
 */

'use strict';

var grunt = require('grunt');
var fs = require('fs');

exports.coffee = {
  compile: function(test) {
    var testSetups = [];

    // Haml-js test setup
    var targets          = ['js', 'html'];
    var inputVariations  = {'js': ['', '_amd'], 'html': ['', '_wrapped']};
    var languages        = ['coffee', 'js'];
    var outputVariations = ['haml', 'concat'];

    for (var i = 0; i < targets.length; i++) {
      var target = targets[i];
      for (var j = 0; j < languages.length; j++) {
        var language = languages[j];
        for (var m = 0; m < outputVariations.length; m++) {
          var outputVariation = outputVariations[m];
          for (var n = 0; n < inputVariations[target].length; n++) {
            var inputVariation = inputVariations[target][n];
            var name = language + '_' + target + inputVariation;
            name += '/' + outputVariation + '.' + target;
            testSetups.push({
              actual: grunt.file.read('tmp/' + name),
              expected: grunt.file.read('test/expected/' + name),
              language: language,
              target: target,
              wrapHtmlInJs: inputVariation === '_wrapped'
            });
          }
        }
      }
    }

    testSetups.push({
      actual: grunt.file.read('tmp/coffee_html_partial/haml.html'),
      expected: grunt.file.read('test/expected/coffee_html_partial/haml.html'),
      language: 'coffee',
      target: 'html'
    });

    testSetups.push({
      actual: grunt.file.read('tmp/ruby_html_wrapped/haml.html'),
      expected: grunt.file.read('test/expected/ruby_html_wrapped/haml.html'),
      language: 'ruby',
      target: 'html'
    });

    testSetups.push({
      actual: grunt.file.read('tmp/ruby_html_wrapped/concat.html'),
      expected: grunt.file.read('test/expected/ruby_html_wrapped/concat.html'),
      language: 'ruby',
      target: 'html'
    });

    // Ruby haml test setups
    for (var x = 0; x < outputVariations.length; x++) {
      var variation = outputVariations[x];
      var ruby_test_name = 'ruby_html/' + variation + '.html';
      testSetups.push({
        actual: grunt.file.read('tmp/' + ruby_test_name),
        expected: grunt.file.read('test/expected/' + ruby_test_name),
        language: 'ruby',
        target: 'html'
      });
    }

    // Run all the tests we prepared
    test.expect(testSetups.length);
    for (i = 0; i < testSetups.length; i++) {
      var testSetup = testSetups[i];
      test.equal(testSetup.actual, testSetup.expected,
        'should compile haml on ' + testSetup.language + ' to ' + testSetup.target);
    }
    test.done();
  }
};
