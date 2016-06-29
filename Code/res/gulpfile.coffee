gulp        = require 'gulp'
concat      = require 'gulp-concat'
debug       = require 'gulp-debug'
del         = require 'del'
streamqueue = require 'streamqueue'
expect      = require 'gulp-expect-file'
imagemin    = require 'gulp-imagemin'
less        = require 'gulp-less'
minifycss   = require 'gulp-minify-css'
rename      = require 'gulp-rename'
uglify      = require 'gulp-uglify'

cfg_npm          = 'node_modules/'
cfg_wch_root     = '../webapp-client-home/'
cfg_wcp_root     = '../webapp-client-project/'
cfg_ws_root      = '../webapp-server/'
cfg_ws_webapp    = cfg_ws_root + 'src/main/webapp/'
cfg_ws_dev       = cfg_ws_webapp + 'dev/'
cfg_ws_prod      = cfg_ws_webapp + 'a/'
cfg_ws_customCss = cfg_ws_root + 'src/main/styles/'

nonRetardedSrc = (a) -> gulp.src(a).pipe expect a

devProdTasks = (name, srcs, mapBoth, mapDevTask, mapProdTask) ->
  nameD = name + ':dev'
  nameP = name + ':prod'

  mkSrc = (dev) ->
    devprod = (prefix, d = '', p = '.min', suffix = '.js') -> prefix + (if dev then d else p) + suffix
    nonRetardedSrc(srcs(devprod))
      .pipe(debug({title: (if dev then nameD else nameP) + ' ←'}))

  gulp.task(nameD, -> mapDevTask  mapBoth mkSrc true)
  gulp.task(nameP, -> mapProdTask mapBoth mkSrc false)
  gulp.task name, [nameD, nameP]

devProdJs = (name, outfile, srcs) ->
  b = (b) -> b.pipe concat outfile
  d = (d) -> d.pipe gulp.dest cfg_ws_dev
  # p = (p) -> p.pipe gulp.dest cfg_ws_prod
  p = (p) -> p.pipe(uglify()).pipe gulp.dest cfg_ws_prod
  devProdTasks(name, srcs, b, d, p)

# ======================================================================================================================
# webapp-server

gulp.task 'ws:clean', ->
  dirs = [cfg_ws_dev, cfg_ws_prod]
  del (d + '**/*' for d in dirs), force: true

gulp.task 'ws:vendor:1', ->
  nonRetardedSrc [
      cfg_npm + 'katex/dist/**/*'
      '!**/*.md'
      'vendor/**/*'
    ]
    .pipe gulp.dest cfg_ws_dev
    .pipe gulp.dest cfg_ws_prod

gulp.task 'ws:vendor:2', ->
  nonRetardedSrc ['semantic/dist/themes/default/assets/fonts/**/*']
    .pipe gulp.dest cfg_ws_dev + '/themes/default/assets/fonts'
    .pipe gulp.dest cfg_ws_prod + '/themes/default/assets/fonts'

gulp.task 'ws:vendor', [], ->
  gulp.start ['ws:vendor:1', 'ws:vendor:2']

devProdJs 'ws:public', 'public-deps.js', (f) ->
  [
    cfg_npm + 'jquery/dist/jquery.min.js'
  ]

gulp.task 'ws:member:css', [], ->
  semantic = -> nonRetardedSrc ['semantic/dist/semantic.min.css']
  custom   = -> nonRetardedSrc [
               'custom-css/textcomplete.css'
               'custom-css/semantic-fixes.css'
             ]
  name = 'member.css'
  # Note: semantic MUST come first else the font @import won't work.
  dev = streamqueue({objectMode: true}, semantic(), custom())
    .pipe concat name
    .pipe gulp.dest cfg_ws_dev
  prod = streamqueue({objectMode: true}, semantic(), custom() .pipe minifycss())
    .pipe concat name
    .pipe gulp.dest cfg_ws_prod
  streamqueue({objectMode: true}, dev, prod)

devProdJs 'ws:member:init', 'member-deps-init.js', (f) ->
  [
    f(cfg_npm + 'loadjs/dist/loadjs')
      cfg_npm + 'jquery/dist/jquery.min.js'
    f('semantic/dist/semantic')
  ]

devProdJs 'ws:member:next', 'member-deps-next.js', (f) ->
  [
    f(cfg_npm + 'autosize/dist/autosize')
    f(cfg_npm + 'jquery-textcomplete/dist/jquery.textcomplete')
      cfg_npm + 'moment/min/moment.min.js'
    f(cfg_npm + 'react/dist/react', '-with-addons')
    f(cfg_npm + 'react-dom/dist/react-dom')
    f(cfg_npm + 'react-dom/dist/react-dom-server')
      cfg_npm + 'react-motion/build/react-motion.js'
      cfg_npm + 'react-height/build/react-height.js'
    f(cfg_npm + 'react-collapse/build/react-collapse')
  ]

gulp.task 'ws:member', [], ->
  gulp.start ['ws:member:css', 'ws:member:init', 'ws:member:next']

gulp.task 'ws:css', ->
  nonRetardedSrc cfg_ws_customCss + '*.less'
    .pipe less()
    .pipe gulp.dest cfg_ws_dev
    .pipe minifycss()
    .pipe gulp.dest cfg_ws_prod

gulp.task 'ws:images', ->
  gulp.src 'custom-images/**/*'
    .pipe imagemin()
    .pipe gulp.dest cfg_ws_dev
    .pipe gulp.dest cfg_ws_prod

gulp.task 'ws:js', [], ->
  gulp.start ['ws:public', 'ws:member']

gulp.task 'ws', ['ws:clean'], ->
  gulp.start ['ws:vendor', 'ws:js', 'ws:css', 'ws:images']

# ======================================================================================================================
# webapp-client

# create JS for unit tests
gulp.task 'wc:testjs', ->
  nonRetardedSrc [
        cfg_npm + 'autosize/dist/autosize.min.js'
        cfg_npm + 'jquery/dist/jquery.min.js'
        cfg_npm + 'jquery-textcomplete/dist/jquery.textcomplete.min.js'
        cfg_npm + 'moment/min/moment.min.js'
        cfg_npm + 'react/dist/react-with-addons.js' # not .min because TestUtils is needed
        cfg_npm + 'react-dom/dist/react-dom.min.js'
        cfg_npm + 'react-dom/dist/react-dom-server.min.js'
        cfg_npm + 'react-motion/build/react-motion.js'
        cfg_npm + 'react-height/build/react-height.min.js'
        cfg_npm + 'react-collapse/build/react-collapse.min.js'
        'semantic/dist/semantic.min.js'
      ]
    .pipe concat 'shipreq-client-test.js'
    .pipe uglify()
    .pipe gulp.dest cfg_wch_root + 'src/test/resources'
    .pipe gulp.dest cfg_wcp_root + 'src/test/resources'

gulp.task 'wc', ['wc:testjs']

# ======================================================================================================================
# webapp-gen

gulp.task 'wg', ->
  nonRetardedSrc [
        cfg_npm + 'react/dist/react.js'
        cfg_npm + 'react-dom/dist/react-dom-server.js'
      ]
    .pipe concat 'shipreq-gen-deps.js'
    .pipe gulp.dest '../webapp-gen/js/src/main/resources'

# ======================================================================================================================
gulp.task 'default', ['wc','wg','ws']
