module.exports = function(grunt) {
  'use strict';

  // Load all the Grunt tasks listed in package.json
  require('matchdep').filterDev('grunt-*').forEach( grunt.loadNpmTasks );

  // ===========================================================================
  grunt.initConfig({

    haml: {
      all: {
        files: [{expand: true, cwd: '', src: ['*.haml'], dest: '', ext: '.haml.html', flatten: false }],
      },
    },

    watch: {
      options: {
        spawn: false,
      },
      haml: { files: ['*.haml'], tasks: ['haml'] },
    },

  });

  // Prevent stale reads
  grunt.event.on('watch', function(action, filepath, target) {
    var t = filepath + ".html";
    if (grunt.file.exists(t)) {
      grunt.file.delete(t);
      grunt.log.ok("Deleted: "+t);
    }
  });

  // ===========================================================================
  grunt.registerTask('default', ['haml']);

};

// vim:sw=2 ts=2 et:
