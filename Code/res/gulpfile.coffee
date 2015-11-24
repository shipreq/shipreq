gulp      = require 'gulp'
concat    = require 'gulp-concat'
debug     = require 'gulp-debug'
del       = require 'del'
expect    = require 'gulp-expect-file'
less      = require 'gulp-less'
minifycss = require 'gulp-minify-css'
rename    = require 'gulp-rename'
uglify    = require 'gulp-uglify'

cfg_bower        = 'bower_components/'
cfg_wc_root      = '../webapp-client/'
cfg_ws_root      = '../webapp-server/'
cfg_ws_webapp    = cfg_ws_root + 'src/main/webapp/'
cfg_ws_dev       = cfg_ws_webapp + 'dev/'
cfg_ws_prod      = cfg_ws_webapp + 'a/'
cfg_ws_customJs  = cfg_ws_root + 'src/main/javascript/'
cfg_ws_customCss = cfg_ws_root + 'src/main/styles/'

nonRetardedSrc = (a) -> gulp.src(a).pipe expect a

devProdTasks = (name, srcs, mapBoth, mapDevTask, mapProdTask) ->
  mkSrc = (dev) ->
    devprod = (prefix, d = '', p = '.min', suffix = '.js') -> prefix + (if dev then d else p) + suffix
    nonRetardedSrc srcs devprod
  nameD = name + ':dev'
  nameP = name + ':prod'
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
#
# TODO: bootstrap.less expects Dejavu fonts. From memory it was so that the UC arrows work on windows.

gulp.task 'ws:clean', ->
  dirs = [cfg_ws_dev, cfg_ws_prod]
  del (d + '**/*' for d in dirs), force: true

gulp.task 'ws:vendor', ->
  nonRetardedSrc [
      cfg_bower + 'katex/dist/**/*'
      '!**/*.md'
      cfg_bower + 'zeroclipboard/ZeroClipboard.swf'
    ]
    .pipe gulp.dest cfg_ws_dev
    .pipe gulp.dest cfg_ws_prod

devProdJs 'ws:anon', 'anon.js', (f) ->
  [
    cfg_ws_customJs + 'google-analytics.js'
    cfg_bower + 'jquery/dist/jquery.min.js'
    cfg_bower + 'bootstrap/js/alert.js'
    cfg_bower + 'bootstrap/js/dropdown.js'
    cfg_bower + 'bootstrap/js/modal.js'
    cfg_bower + 'bootstrap/js/tab.js'
    cfg_bower + 'bootstrap/js/transition.js'
    cfg_bower + 'jquery.ui/ui/jquery.ui.core.js'
    cfg_bower + 'jquery.ui/ui/jquery.ui.effect.js'
    cfg_bower + 'jquery.ui/ui/jquery.ui.effect-drop.js'
    cfg_bower + 'jquery.ui/ui/jquery.ui.effect-fade.js'
    cfg_bower + 'jquery.ui/ui/jquery.ui.effect-highlight.js'
    cfg_bower + 'jquery.ui/ui/jquery.ui.effect-slide.js'
    cfg_bower + 'jquery-autosize/jquery.autosize.min.js'
    cfg_bower + 'jquery-timeago/jquery.timeago.js'
    cfg_bower + 'jquery.livequery/dist/jquery.livequery.min.js'
    cfg_bower + 'jquery-rangyinputs/rangyinputs-jquery.js'
    cfg_bower + 'mousetrap/mousetrap.min.js'
    cfg_bower + 'mousetrap/plugins/global-bind/mousetrap-global-bind.min.js'
  ]

devProdJs 'ws:project', 'project.js', (f) ->
  [
    cfg_bower + 'jquery/dist/jquery.min.js'
    f(cfg_bower + 'react/react', '-with-addons')
    f(cfg_bower + 'react/react-dom')
    f(cfg_bower + 'jquery-textcomplete/dist/jquery.textcomplete')
  ]

gulp.task 'ws:css', ->
  nonRetardedSrc cfg_ws_customCss + '*.less'
    .pipe less paths: [cfg_bower + 'bootstrap/less', cfg_ws_customCss + 'include']
    .pipe gulp.dest cfg_ws_dev
    .pipe minifycss()
    .pipe gulp.dest cfg_ws_prod

gulp.task 'ws:images', ->
  gulp.src 'images/**/*'
    .pipe gulp.dest cfg_ws_dev
    .pipe gulp.dest cfg_ws_prod

gulp.task 'ws', ['ws:clean'], ->
  gulp.start ['ws:vendor', 'ws:anon', 'ws:project', 'ws:css', 'ws:images']

# ======================================================================================================================
# webapp-client

# create JS for unit tests
gulp.task 'wc:testjs', ->
  nonRetardedSrc [
        cfg_bower + 'sizzle/dist/sizzle.min.js'
        cfg_bower + 'jquery/dist/jquery.min.js'
        cfg_bower + 'jquery-textcomplete/dist/jquery.textcomplete.min.js'
        cfg_bower + 'react/react-with-addons.js'
        cfg_bower + 'react/react-dom.min.js'
      ]
    .pipe concat 'test.js'
    .pipe uglify()
    .pipe gulp.dest cfg_wc_root + 'src/test/resources'

gulp.task 'wc', ['wc:testjs']

# ======================================================================================================================
gulp.task 'default', ['wc','ws']
