const path = require('path')
const express = require('express')
const errorhandler = require('errorhandler')

const app = express()
const port = 3000

const dev = process.env.NODE_ENV == 'development'
const prod = !dev

// This shit is mostly copied from https://github.com/dai-shi/easy-livereload
function liveReload() {
  const livereload = require('easy-livereload')

  var file_type_map = {
    pug: 'html', // `index.pug` maps to `index.html`
    js: 'js',
    jsx: 'jsx',
    css: 'css',
    svg: 'svg',
    // add the file type being edited and what you want it to be mapped to.
  }

  // store the generated regex of the object keys
  var file_type_regex = new RegExp('\\.(' + Object.keys(file_type_map).join('|') + ')$')

  return livereload({
    app: app,
    watchDirs: [
      path.join(__dirname, 'src'),
      path.join(__dirname, 'static'),
    ],
    checkFunc: function(file) {
      return file_type_regex.test(file)
    },
    renameFunc: function(file) {
      // remap extension of the file path to one of the extensions in `file_type_map`
      return file.replace(file_type_regex, function(extension) {
        return '.' + file_type_map[extension.slice(1)]
      })
    },
  })
}

app.get('/', (req, res) => {
  // res.send('/xxx => ./views/xxx.pug')
  // res.render('index') // Doing this doesn't work with live-reload
  res.redirect('/index')
})

const serveViews = function (req, res, next) {
  const m = req.url.match(/^\/?([^.]+)$/)
  if (m)
    res.render(m[1])
  else
    next()
}

// Generate minified output
app.locals.pretty = dev

app.set('view engine', 'pug')
app.set('views', path.join(__dirname, 'src'))

app.use(express.static('static'))
app.use('/node_modules', express.static('node_modules'))
if (dev) app.use(liveReload());
app.use(serveViews)
app.use(errorhandler())

app.listen(port, () => {
  console.log(`Listening at http://localhost:${port}`)
})
