const
  CamelCase = require('camelcase'),
  Deasync = require('deasync'),
  EscapeRegexp = require('escape-string-regexp'),
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

// SJS resources all go in /j/ as is configured in web.xml
const sjsDir = 'j';

const makeConfig = ({ mode, name, sjsName, staticDir, htmlMinifyOptions }) => {

  const sjs = (name, manifest) => ({
    type: 'external',
    path: `${sjsDir}/${sjsName(name)}`,
    manifest,
  });

  const svgo = new Svgo({
    plugins: [{
      removeViewBox: false,
    }],
  });
  const webpackOutput = `/tmp/shipreq.webpack.${mode}`;
  const dotMin = mode == 'dev' ? '' : '.min';

  const addSlash = d => (!d || /\/$/.test(d)) ? d : `${d}/`;

  const fromWebpack = o => Object.assign({
    type: 'local',
    src: webpackOutput,
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

  return {

    src: Path.resolve(__dirname, ".."),

    output: {
      dir: `dist/${mode}/serve`,
      name: `${staticDir}/${name}`,
      manifest: false,
    },

    assets: {

      html: { type: 'local', src: 'src/html', files: '**/*.html', outputName: '[path]/[basename]' },

      favicon: { type: 'local', src: 'src/images', files: 'favicon.ico', manifest: true },

      images: { type: 'local', src: 'src/images', files: '*.{svg,png}', manifest: CamelCase },

      webappClientPublic: [
        sjs('public', 'webappClientPublicJs'),
        'publicLibs',
      ],

      webappClientHome: [
        sjs('home', 'webappClientHomeJs'),
        'memberLibs',
      ],

      webappClientProject: [
        sjs('project', 'webappClientProjectJs'),
      ],

      webappClientWw: [
        sjs('ww', 'webappClientWwJs'),
        'vizJs',
      ],

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
        fromCdnjs({npm:'prismjs', cdn:'prism'}, 'prism-match-braces.min.js', {manifest: 'prismJsMatchBraces',    path: 'plugins/match-braces'}),
        fromCdnjs({npm:'prismjs', cdn:'prism'}, 'prism-match-braces.css',    {manifest: 'prismJsMatchBracesCss', path: 'plugins/match-braces'}),
      ],

      vizJs: { type: 'local', files: 'vendor/viz.js', manifest: true },

      vizWasm: { type: 'local', files: 'vendor/viz.wasm', manifest: true },

      loadjs: { type: 'local', files: `node_modules/loadjs/dist/loadjs${dotMin}.js`, manifest: true },
    },

    plugins: [

      // Remove @import from Semantic UI
      Webtamp.plugins.Modify.content(
        /semantic.*css$/,
        c => c.replace(new RegExp(`@import *url *\\(['" ]*${EscapeRegexp(semanticUiImport)}['" ]*\\)[ ;]*`), ''),
        { failUnlessChange: true }
      ),

      // Minify SVGs
      Webtamp.plugins.Modify.content(/\.svg$/, c => svgoOptimizeSync(svgo, c)),

      // Inline small images
      Webtamp.plugins.Inline.data(i => /\.(svg|png)$/.test(i.dest) && i.size() < 4096),

      // Replace <require> tags and webtamp:// URIs
      Webtamp.plugins.Html.replace({ modTag: fixLinksInLiftTemplates }),

      // Minify HTML
      htmlMinifyOptions && Webtamp.plugins.Html.minify({ options: htmlMinifyOptions }),

      // Manifest for Scala
      Webtamp.plugins.Manifest.extractCss({}),
      Webtamp.plugins.Manifest.generate.scala({
        object: "shipreq.webapp.base.AssetManifest",
        outputPath: '../scala',
      }),

      // Build AnalyticsConfig.scala
      Webtamp.plugins.Modify.content(
        /AnalyticsConfig\.scala/,
        c => {
          const a = [];
          a.push("package shipreq.webapp.base");
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
