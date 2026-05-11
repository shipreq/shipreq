const
  CamelCase = require('camelcase'),
  Deasync = require('deasync'),
  EscapeRegexp = require('escape-string-regexp'),
  FS = require('fs'),
  Path = require('path'),
  Svgo = require('svgo'),
  Webtamp = require(process.env.WEBTAMP ? `${process.env.WEBTAMP}/src/main` : 'webtamp');


const moduleVer = n => require(`../node_modules/${n}/package.json`).version;

const fixLinksInLiftTemplates = i =>
  /<head[ >]/.test(i.content()) ? t => t :
  tag => tag.replace(/^(<link )/, '$1data-lift="head" ');

function svgoOptimizeSync(svgo, content) {
  let res;
  svgo.optimize(content).then(result => res = result);
  Deasync.loopWhile(() => !res);
  if (res.error) throw Error(res.error)
  return res.data;
}

const semanticUiImport = 'https://fonts.googleapis.com/css?family=Lato:400,700,400italic,700italic&subset=latin';

const faviconHead = FS.readFileSync('src/favicon/head.html').toString().trim()

const makeConfig = ({ mode, name, staticDir, htmlMinifyOptions }) => {

  const svgo = new Svgo({
    plugins: [{
      removeViewBox: false,
    }],
  });
  const webpackOutput = `/tmp/shipreq.webpack.${mode}`;
  const webpackOutputWW = `/tmp/shipreq.webpack.${mode}.ww`;
  const dotMin = mode == 'dev' ? '' : '.min';

  const addSlash = d => (!d || /\/$/.test(d)) ? d : `${d}/`;

  const fromWebpack = o => Object.assign({
    type: 'local',
    src: webpackOutput,
    outputPath: staticDir,
    outputName: name,
  }, o);

  const fromWebpackWW = o => Object.assign({
    type: 'local',
    src: webpackOutputWW,
    outputPath: staticDir,
    outputName: name,
  }, o);

  /**
    * @param lib      npm package name as in ./node_modules/xxx
    * @param filename asset filename without any directory info.
    *                 Use opts.path if to specify the directory from the npm package root.
    */
  const fromCdnjs = (lib, filename, opts) => {
    const libC       = lib.cdn || lib;
    const libN       = lib.npm || lib;
    const pathO      = opts.path || {};
    const optPathStr = typeof(opts.path) === 'string' && opts.path
    const pathC      = addSlash(pathO.cdn || optPathStr || '');
    const pathN      = addSlash(pathO.npm || optPathStr || 'dist');

    const name       = libC;
    const ver        = moduleVer(libN);
    const path       = `${pathC}${filename}`;

    const cloudfare = `https://cdnjs.cloudflare.com/ajax/libs/${name}/${ver}/${path}`;
  //const jsdelivr = `https://cdn.jsdelivr.net/npm/${name}@${ver}/${path}`;
  //const unpkg    = `https://unpkg.com/${name}@${ver}/${path}`;

    return {
      type: 'cdn',
      url: cloudfare,
      integrity: { files: `node_modules/${libN}/${pathN}${filename}` },
      manifest: opts.manifest,
    };
  };

  const reactLib = (lib, basename, opts) => {
    const suffix   = mode == 'dev' ? 'development.js' : 'production.min.js';
    const filename = `${basename}.${suffix}`;
    return fromCdnjs(lib, filename, Object.assign(opts, {path: 'umd'}));
  };

  const indent = (by, str) => by + str.replace(/\n/g, "\n" + by);

  const jsConstObjToScala = (str, jsName) =>
    new RegExp(`const +${jsName}[^}]+\}`).exec(str)[0]
      .replace("const ", "object ")
      .replace(/ *= *\{/, " {")
      .replace(/^( +)([a-zA-Z])/mg, "$1def $2")
      .replace(/:/g, " =")
      .replace(/'/g, '"')
      .replace(/, *$/mg, "");

  function faviconManifest(i) {
    const s = /favicon.*/.test(i) ? i : i = "favicon-" + i
    return CamelCase(s)
  }

  return {

    src: Path.resolve(__dirname, ".."),

    output: {
      dir: `dist/${mode}/serve`,
      name: `${staticDir}/${name}`,
      manifest: false,
    },

    assets: {

      html: { type: 'local', src: 'src/html', files: '**/*.html', outputName: '[path]/[basename]' },

      // These are included in the manifest so that they can be referenced from site.webmanifest, browserconfig.xml, and html files
      faviconImages: { type: 'local', src: 'src/favicon', files: '*.{ico,png,svg}', manifest: faviconManifest },
      faviconManifests: { type: 'local', src: 'src/favicon', files: '*.{webmanifest,xml}', manifest: faviconManifest },

      images: { type: 'local', src: 'src/images', files: '*.{svg,png}', manifest: CamelCase },

      analytics: fromWebpack({ files: 'analytics.js', manifest: CamelCase }),

      analyticsConfig: { type: 'local', src: 'src/js', files: 'analytics.js', outputPath: '../scala', outputName: 'AnalyticsConfig.scala' },

      // ---------------------------------------------------------------------------------------------------------------
      // BE ADVISED that when you changes these bundles, you may need to change how assets are used from Scala.
      // grep Scala source code for AssetManifest.

      spaWithLoader: [
        'jquery', // Lift needs this immediately
        'semanticCss',
        'loadjs',
      ],

      semanticCss: [
        { type: 'cdn', url: semanticUiImport, as: 'style' },
        fromWebpack({ files: 'semantic*.css', manifest: CamelCase }),
        fromWebpack({ files: 'icons.*', transitive: true }),
      ],

      semanticJs: fromWebpack({ files: 'semantic*.js', manifest: CamelCase }),

      semantic: ['semanticCss', 'semanticJs'],

      publicLibs: [
        'react',
        'jquery',
        'semantic',
      ],

      memberLibs: [
        'reactDomSvr',
        fromWebpack({ files: 'member-lib-bundle.js', manifest: CamelCase }),
        'jquery',
        'semantic',
        'katex',
      ],

      jquery: fromCdnjs('jquery', 'jquery.min.js', {manifest: 'jqueryJs', path: {npm: 'dist'}}),

      react: [
        reactLib('react', 'react', {manifest: 'reactJs'}),
        reactLib('react-dom', 'react-dom', {manifest: 'reactDomJs'}),
      ],

      reactDomSvr: [
        'react',
        reactLib('react-dom', 'react-dom-server.browser', {manifest: 'reactDomServerJs'}),
      ],

      katex: [
        fromCdnjs({npm:'katex', cdn:'KaTeX'}, 'katex.min.js', {manifest: 'katexJs', path: {npm: 'dist'}}),
        fromCdnjs({npm:'katex', cdn:'KaTeX'}, 'katex.min.css', {manifest: 'katexCss', path: {npm: 'dist'}}),
        // { type: 'local', src: 'node_modules/katex/dist', files: '*.min.{js,css}' },
        // { type: 'local', src: 'node_modules/katex/dist', files: 'fonts/**/*', transitive: true },
        // { type: 'local', src: 'node_modules/katex/dist', files: 'images/**/*', transitive: true },
      ],

      prismJs: [
        fromCdnjs({npm:'prismjs', cdn:'prism'}, 'prism-core.min.js',         {manifest: 'prismJsCore',           path: 'components'}),
        fromCdnjs({npm:'prismjs', cdn:'prism'}, 'prism-okaidia.css',         {manifest: 'prismJsCss',            path: 'themes'}),
        fromCdnjs({npm:'prismjs', cdn:'prism'}, 'prism-autoloader.min.js',   {manifest: 'prismJsAutoloader',     path: 'plugins/autoloader'}),
        fromCdnjs({npm:'prismjs', cdn:'prism'}, 'prism-line-numbers.min.js', {manifest: 'prismJsLineNumbers',    path: 'plugins/line-numbers'}),
        fromCdnjs({npm:'prismjs', cdn:'prism'}, 'prism-line-numbers.css',    {manifest: 'prismJsLineNumbersCss', path: 'plugins/line-numbers'}),
        fromCdnjs({npm:'prismjs', cdn:'prism'}, 'prism-match-braces.min.js', {manifest: 'prismJsMatchBraces',    path: 'plugins/match-braces'}),
        fromCdnjs({npm:'prismjs', cdn:'prism'}, 'prism-match-braces.css',    {manifest: 'prismJsMatchBracesCss', path: 'plugins/match-braces'}),
      ],

      vizJs: { type: 'local', files: 'vendor/viz.js', manifest: true },

      vizWasm: { type: 'local', files: 'vendor/viz.wasm', manifest: true },

      ww: fromWebpackWW({ files: 'ww.js', manifest: CamelCase }),

      loadjs: { type: 'local', files: `node_modules/loadjs/dist/loadjs${dotMin}.js`, manifest: true },
    },

    plugins: [

      // Remove @import from Semantic UI
      Webtamp.plugins.Modify.content(
        /semantic.*css$/,
        c => c.replace(new RegExp(`@import *url *\\(['" ]*${EscapeRegexp(semanticUiImport)}['" ]*\\)[ ;]*`), ''),
        { failUnlessChange: true }
      ),

      // Replace <webtamp.favicon/> in HTML
      Webtamp.plugins.Modify.content(
        /\.html$/,
        c => c.replace(/< *webtamp *\. *favicon *\/? *>/ig, faviconHead),
        { failUnlessChange: false }
      ),

      // Minify SVGs
      Webtamp.plugins.Modify.content(/\.svg$/, c => svgoOptimizeSync(svgo, c)),

      // Inline small images
      Webtamp.plugins.Inline.data(i =>
        /\.(svg|png)$/.test(i.dest) &&
        !/favicon/i.test(i.manifestName) && // don't inline favicons
        i.size() < 4096
      ),

      // Replace <require> tags and webtamp:// URIs in HTML
      Webtamp.plugins.Html.replace({ modTag: fixLinksInLiftTemplates }),

      // Replace webtamp:// URIs in non-HTML
      Webtamp.plugins.Modify.replaceWebtampUrls({
        testFilename: /(browserconfig.xml|site.webmanifest)$/,
        urlQuotes: [`"`],
      }),

      // Minify HTML
      htmlMinifyOptions && Webtamp.plugins.Html.minify({ options: htmlMinifyOptions }),

      // Manifest for Scala
      Webtamp.plugins.Manifest.extractCss({}),
      Webtamp.plugins.Manifest.generate.scala({
        object    : "shipreq.webapp.base.config.AbstractAssetManifest",
        outputPath: '../scala',
        abstract  : true,
      }),

      // Build AnalyticsConfig.scala
      Webtamp.plugins.Modify.content(
        /AnalyticsConfig\.scala/,
        c => {
          const a = [];
          a.push("package shipreq.webapp.base.config");
          a.push("");
          a.push("/** Generated by webtamp from analytics.js. */");
          a.push("object AnalyticsConfig {");
          a.push("");
          a.push(indent("  ", jsConstObjToScala(c, 'dimensions')));
          a.push("");
          a.push(indent("  ", jsConstObjToScala(c, 'metrics')));
          a.push("}");
          return a.join("\n");
        },
        { failUnlessChange: true }
      ),
    ],
  }
};

module.exports = makeConfig;
