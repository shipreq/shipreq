module.exports = function(grunt) {
  'use strict';

  // Load all the Grunt tasks listed in package.json
  require('matchdep').filterDev('grunt-*').forEach( grunt.loadNpmTasks );

  function jsConcat(name) {
    return '<%= cfg.js.tmp %>/' + name + '.con.js';
  }

  function jsConcatTask(name, srcs) {
    srcs = srcs || []
    srcs.push('<%= cfg.js.src %>/' + name + '.js');
    return { src: srcs, dest: jsConcat(name), nonull: true };
  }

  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),

    cfg: {
      css: {
        src: 'src/main/styles',
        out: 'src/main/webapp/css',
        tmp: 'target/css',
        bootstrap_cust: 'vendor/boostrap.css',
        app_only: '<%= cfg.css.tmp %>/app-only.css',
      },
      js: {
        src: 'src/main/javascript',
        out: 'src/main/webapp/js',
        tmp: 'target/js',
      },
    },

    // TODO ZeroClipboard.swf shouldn't be in js/vendor

    // *****************************************************************************************************************
    clean: {

      // Delete temp dirs
      tmp: ['<%= cfg.js.tmp %>', '<%= cfg.css.tmp %>'],

      // Delete previously-generated CSS
      css: ['<%= cfg.css.out %>'],

      // Delete previously-generated JS
      js: {
        expand: true,
        cwd: '<%= cfg.js.out %>',
        src: ['**/*', '!vendor/mathjax/**/*', '!vendor/viz.js', '!vendor/jquery.js', '!vendor/ZeroClipboard.swf'],
        filter: 'isFile',
      }
    },

    // *****************************************************************************************************************
    copy: {

      // Copies required 3rd-party files
      vendor: {
        files: [
          {dest:'src/main/webapp/js/vendor/jquery.js',         src:'.bower/jquery/jquery.min.js',            nonull:true},
          {dest:'src/main/webapp/js/vendor/ZeroClipboard.swf', src:'.bower/zeroclipboard/ZeroClipboard.swf', nonull:true},
        ]
      },
    },

    // *****************************************************************************************************************
    concat: {

      // Merge JS files before processing
      js: {
        files: [
            jsConcatTask('app', [
              // jquery is loaded via CDN, else it would be here too. TODO Change?
              '.bower/bootstrap/js/alert.js',
              '.bower/bootstrap/js/dropdown.js',
              '.bower/bootstrap/js/modal.js',
              '.bower/bootstrap/js/tab.js',
              '.bower/bootstrap/js/transition.js',
              '.bower/jquery.ui/ui/jquery.ui.core.js',
              '.bower/jquery.ui/ui/jquery.ui.effect.js',
              '.bower/jquery.ui/ui/jquery.ui.effect-drop.js',
              '.bower/jquery.ui/ui/jquery.ui.effect-fade.js',
              '.bower/jquery.ui/ui/jquery.ui.effect-highlight.js',
              '.bower/jquery.ui/ui/jquery.ui.effect-slide.js',
              '.bower/jquery-autosize/jquery.autosize.min.js',
              '.bower/jquery-timeago/jquery.timeago.js',
              '.bower/jquery.livequery/dist/jquery.livequery.min.js',
              '.bower/jquery-rangyinputs/rangyinputs-jquery.js',
              '.bower/mousetrap/mousetrap.min.js',
              '.bower/mousetrap/plugins/global-bind/mousetrap-global-bind.min.js',
            ]),
          jsConcatTask('zeroclipboard', [
            '.bower/zeroclipboard/ZeroClipboard.js',
          ]),
          {
            expand: true,
            cwd : '<%= cfg.js.src %>',
            dest: '<%= cfg.js.tmp %>',
            src: ['**/*.js', '!app.js', '!zeroclipboard.js'],
          },
        ]
      },

      // Combine 3rd-party CSS with our app.css
      app_css: {
        nonull: true,
        src: [ '<%= cfg.css.bootstrap_cust %>', '<%= cfg.css.app_only %>'],
        dest: '<%= cfg.css.out %>/app.css',
      },
    },

    // *****************************************************************************************************************
    uglify: {
      options: {
        // mangle: false,
        preserveComments: false,
      },

      // Shrink JS files
      all: {
        files: [{
          expand: true,
          cwd : '<%= cfg.js.tmp %>',
          dest: '<%= cfg.js.out %>',
          src: '**/*.js',
          ext: '.js',
        }]
      },
    },

    // *****************************************************************************************************************
    less: {

      // Build custom bootstrap.css
      bootstrap: {
        options: {
          paths: '.bower/bootstrap/less',
          // compress: true,
          cleancss: true,
          // optimization: 2,
          // strictMath: true, // Pending 3.1.0
        },
        src: ['<%= cfg.css.src %>/bootstrap.less'],
        dest: '<%= cfg.css.bootstrap_cust %>',
      },
    },

    // *****************************************************************************************************************
    // TODO sass requires ruby, just switch over to less. It's already required for bootstrap.
    sass: {
      options: {style: 'compact'},

      // Generate app.css but keep separate because it will be merged with 3rd-party CSS later
      app: {
        nonull: true,
        src: '<%= cfg.css.src %>/app.scss',
        dest: '<%= cfg.css.app_only %>',
      },

      // Generate CSS
      other: {
        files: [{
          expand: true,
          cwd : '<%= cfg.css.src %>',
          dest: '<%= cfg.css.out %>',
          src: ['**/*.s?ss', '!app.s?ss'],
          ext: '.css',
        }]
      },
    },

    // *****************************************************************************************************************
    cssmin: {
      options: {keepSpecialComments: 0},
      // bootstrap: {
        // nonull: true,
        // src: ['<%= cfg.css.bootstrap_cust %>'],
        // dest: '<%= cfg.css.bootstrap_cust %>'
      // },

      // Shrink CSS files in-place
      all: {
        files: [{
          expand: true,
          cwd : '<%= cfg.css.out %>',
          dest: '<%= cfg.css.out %>',
          src: ['**/*.css'],
          ext: '.css',
        }]
      },
    },

  });

  // *******************************************************************************************************************
  // Task definitions

  // Task #0: clean
  // Task #2: js     - Builds js. Run whenever homemade js changes.
  // Task #3: css    - Builds css. Run whenever homemade css changes.
  grunt.registerTask('vendor' , ['copy:vendor','less:bootstrap'                          ]);
  grunt.registerTask('js'     , ['clean:js'   ,'concat:js'     ,'uglify'                 ]);
  grunt.registerTask('css'    , ['clean:css'  ,'sass'          ,'concat:app_css','cssmin']);
  grunt.registerTask('default', ['clean:tmp'  ,'vendor'        ,'js'            ,'css'   ]);
};

// vim:sw=2 ts=2 et:
