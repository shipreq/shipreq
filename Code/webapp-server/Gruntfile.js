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

  var cssminSpec = {
    expand: true,
    cwd : '<%= cfg.css.tmp %>',
    dest: '<%= cfg.css.out %>',
    src: ['**/*.css', '!app-only*'],
    ext: '.css',
  }

  var jQueryVersion = grunt.file.readJSON('.bower/jquery/bower.json').version;

  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),

    cfg: {
      assets: 'src/main/webapp/assets',
      assets_dev: '<%= cfg.assets %>/dev',
      css: {
        src: 'src/main/styles',
        out: '<%= cfg.assets %>',
        tmp: 'target/css',
        bootstrap_cust: '<%= cfg.vendor.cust %>/bootstrap.css',
        app_only: '<%= cfg.css.tmp %>/app-only.css',
      },
      js: {
        src: 'src/main/javascript',
        out: '<%= cfg.assets %>',
        tmp: 'target/js',
      },
      vendor: {
        cust: 'vendor',
        out: '<%= cfg.assets %>/vendor',
      },
      mathjax: {
        src: '.bower/MathJax',
        out: '<%= cfg.vendor.out %>/mathjax',
      },
    },

    // *****************************************************************************************************************
    clean: {

      // Delete temp dirs
      css_tmp: ['<%= cfg.css.tmp %>'],
      js_tmp: ['<%= cfg.js.tmp %>'],

      // Delete dev assets
      dev: {
        expand: true,
        cwd: '<%= cfg.assets_dev %>',
        src: ['**/*', '!webapp-client*'],
        filter: 'isFile',
      },

      // Delete vendor assets
      vendor: {
        expand: true,
        cwd: '<%= cfg.vendor.out %>',
        src: ['**/*', '!mathjax/**/*'],
        filter: 'isFile',
      },

      // Delete MathJax copy
      mathjax: ['<%= cfg.mathjax.out %>'],

      // Delete previously-generated CSS
      css: {
        expand: true,
        cwd: '<%= cfg.css.out %>',
        src: ['**/*.css', '!dev/**/*', '!vendor/**/*'],
        filter: 'isFile',
      },

      // Delete previously-generated JS
      js: {
        expand: true,
        cwd: '<%= cfg.js.out %>',
        src: ['**/*.js', '!dev/**/*', '!vendor/**/*'],
        filter: 'isFile',
      },
    },

    // *****************************************************************************************************************
    copy: {

      // Copies required 3rd-party files
      vendor: {
        files: [
          {src:'.bower/jquery/dist/jquery.min.js',       dest:'<%= cfg.vendor.out %>/jquery.js',         nonull:true},
          {src:'.bower/jquery/dist/jquery.min.js',       dest:'<%= cfg.assets_dev %>/jquery.js',         nonull:true},
          {src:'.bower/jquery/dist/jquery.min.map',      dest:'<%= cfg.assets_dev %>/jquery.min.map',    nonull:true},
          {src:'.bower/react/react.min.js',              dest:'<%= cfg.vendor.out %>/react.js',          nonull:true},
          {src:'.bower/react/react-with-addons.js',      dest:'<%= cfg.assets_dev %>/react.js',          nonull:true},
          {src:'.bower/zeroclipboard/ZeroClipboard.swf', dest:'<%= cfg.vendor.out %>/ZeroClipboard.swf', nonull:true},
          {src:'<%= cfg.vendor.cust %>/viz.js',          dest:'<%= cfg.vendor.out %>/viz.js',            nonull:true},
        ]
      },

      // Copies MathJax files
      mathjax: {
        files: [{
          expand: true,
          cwd: '<%= cfg.mathjax.src %>',
          src: [
            'extensions/**/*',
            'fonts/HTML-CSS/**/*',
            'images/**/*',
            'jax/element/**/*',
            'jax/input/TeX/**/*',
            'jax/output/HTML-CSS/**/*',
            'localization/en/**/*',
            'MathJax.js',
            // Remove all fonts except Latin-Modern
            '!fonts/HTML-CSS/Asana-*/**/*', '!jax/output/HTML-CSS/fonts/Asana-*/**/*',
            '!fonts/HTML-CSS/Gyre-*/**/*',  '!jax/output/HTML-CSS/fonts/Gyre-*/**/*',
            '!fonts/HTML-CSS/Neo-*/**/*',   '!jax/output/HTML-CSS/fonts/Neo-*/**/*',
            '!fonts/HTML-CSS/STIX-*/**/*',  '!jax/output/HTML-CSS/fonts/STIX-*/**/*',
            '!fonts/HTML-CSS/TeX/**/*',     '!jax/output/HTML-CSS/fonts/TeX/**/*',
            // '!fonts/HTML-CSS/TeX/png/**/*', '!fonts/HTML-CSS/TeX/svg/**/*',
          ],
          dest: '<%= cfg.mathjax.out %>',
        }]
      },

      // Copies CSS files without minimising
      debug_css: { files: [cssminSpec] },
    },

    // *****************************************************************************************************************
    concat: {

      // Merge JS files before processing
      js: {
        files: [
            jsConcatTask('app', [
              // jquery is loaded via CDN, else it would be here too. TODO Change?
              '<%= cfg.js.src %>/google-analytics.js',
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
              '.bower/jquery-textcomplete/dist/jquery.textcomplete.min.js',
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
            src: ['**/*.js', '!app.js', '!google-analytics.js', '!zeroclipboard.js'],
          },
        ]
      },

      // Combine 3rd-party CSS with our app.css
      app_css: {
        nonull: true,
        src: [ '<%= cfg.css.bootstrap_cust %>', '<%= cfg.css.app_only %>'],
        dest: '<%= cfg.css.tmp %>/app.css',
      },
    },

    // *****************************************************************************************************************
    replace: {
      jquery: {
        src:  'src/main/scala/shipreq/webapp/app/AppConfig.scala',
        overwrite: true,
        replacements: [{
          from: /(jQueryVersion *= *")[^"]+/i,
          to: '$1' + jQueryVersion,
        }],
      },
    },

    // *****************************************************************************************************************
    uglify: {
      options: {
        // mangle: false,
        preserveComments: false,
      },

      // Shrink JS files
      own: {
        files: [{
          expand: true,
          cwd : '<%= cfg.js.tmp %>',
          dest: '<%= cfg.js.out %>',
          src: '**/*.js',
          ext: '.js',
        }]
      },

      // Shrink MathJax JS
      mathjax: {
        files: [{
          expand: true,
          cwd : '<%= cfg.mathjax.out %>',
          dest: '<%= cfg.mathjax.out %>',
          src: '**/*.js',
        }]
      },
    },

    // *****************************************************************************************************************
    less: {
        options: {
          paths: '.bower/bootstrap/less',
        },

      // Build custom bootstrap.css
      bootstrap: {
        options: {
          cleancss: true,
          // strictMath: true, // Pending 3.1.0
        },
        src: ['<%= cfg.css.src %>/bootstrap.less'],
        dest: '<%= cfg.css.bootstrap_cust %>',
      },

      // Generate app.css but keep separate because it will be merged with 3rd-party CSS later
      app: {
        nonull: true,
        src: '<%= cfg.css.src %>/app.less',
        dest: '<%= cfg.css.app_only %>',
      },

      // Generate CSS
      other: {
        files: [{
          expand: true,
          cwd : '<%= cfg.css.src %>',
          dest: '<%= cfg.css.tmp %>',
          src: ['**/*.less', '!app.*', '!bootstrap*', '!webfonts.*'],
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

      // Shrink CSS files
      all: { files: [cssminSpec] },
    },

    // *****************************************************************************************************************
    csslint: {
      options: {
        ids: false,                      // disagree
        'unique-headings': false,        // disagree
        'qualified-headings': false,     // disagree
        'overqualified-elements': false, // disagree
        important: false,                // using sparingly
        'box-model': false,              // I'm aware
        'adjoining-classes': false,      // Fuck IE 6/7
        'box-sizing': false,             // Fuck IE 6/7
      },
      all: {
        src: ['<%= cfg.css.tmp %>/**/*.css', '!**/app.css'],
      },
    },

    // *****************************************************************************************************************
    watch: {
      options: {
        spawn: false,
      },
      bootstrap: {
        files: ['<%= cfg.css.src %>/**/*bootstrap*.less'],
        tasks: ['less:bootstrap','css'],
      },
      css: {
        files: ['<%= cfg.css.src %>/**/*.less', '!*bootstrap*'],
        tasks: ['css'],
      },
      js: {
        files: ['<%= cfg.js.src %>/**/*.js'],
        tasks: ['js','qunit'],
      },
      qunit: {
        files: ['src/test/javascript/**/*'],
        tasks: ['qunit'],
      },
    },

    // *****************************************************************************************************************
    qunit: {
      all: ['src/test/javascript/**/*.html'],
    },

  });

  // *******************************************************************************************************************
  // Task definitions

  grunt.registerTask('vendor'  , ['clean:dev', 'clean:vendor', 'copy:vendor', 'less:bootstrap', 'replace:jquery']);
  grunt.registerTask('mathjax' , ['clean:mathjax', 'copy:mathjax', 'uglify:mathjax']);
  grunt.registerTask('js'      , ['clean:js_tmp', 'clean:js', 'concat:js', 'uglify:own']);
  grunt.registerTask('css'     , ['clean:css_tmp', 'clean:css', 'less:app', 'less:other', 'concat:app_css', 'cssmin']); // copy:debug_css
  grunt.registerTask('test'    , ['qunit']);
  grunt.registerTask('default' , ['vendor', 'js', 'css', 'test']);
  grunt.registerTask('all'     , ['mathjax', 'default']);
  grunt.registerTask('lint-css', ['css', 'csslint']);
};

// vim:sw=2 ts=2 et:
