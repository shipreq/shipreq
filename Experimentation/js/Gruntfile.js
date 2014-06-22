module.exports = function(grunt) {
  'use strict';

  // Load all the Grunt tasks listed in package.json
  require('matchdep').filterDev('grunt-*').forEach( grunt.loadNpmTasks );

  // ===========================================================================
  grunt.initConfig({

    haml: {
      all: {
        files: [{expand: true, cwd: 'src/', src: ['**/*.haml'], dest: 'src/', ext: '.html', flatten: false }],
      },
    },

    coffee: {
      all: {
        files: [{expand: true, cwd: 'src/', src: ['**/*.coffee'], dest: 'src/', ext: '-coffee.js', flatten: false }],
      },
    },

    purescriptFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs.hs"
    ],


    psc: {
      all: {
        src: ["<%= purescriptFiles %>"],
        dest: 'src/all-purescript.js',
        // files: [{expand: true, cwd: 'src/', src: ['**/*.purs'], dest: 'src/', ext: '-psc.js', flatten: false }],
      },
    },

    watch: {
      options: {
        spawn: false,
      },
      haml:   { files: ['src/**/*.haml'],   tasks: ['haml'] },
      coffee: { files: ['src/**/*.coffee'], tasks: ['coffee'] },
      psc:    { files: ['src/**/*.purs'],   tasks: ['psc'] },
    },

  });

  // ===========================================================================
  grunt.registerTask('default', ['haml', 'coffee']);

};

// vim:sw=2 ts=2 et:
