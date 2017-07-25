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
  svgo.optimize(content, result => res = result);
  Deasync.loopWhile(() => !res);
  if (res.error) throw Error(res.error)
  return res.data;
}

const semanticUiImport = 'https://fonts.googleapis.com/css?family=Lato:400,700,400italic,700italic&subset=latin';

const makeConfig = ({ mode, name, sjsPath, htmlMinifyOptions }) => {

  const svgo = new Svgo();
  const webpackOutput = `/tmp/shipreq.webpack.${mode}`;
  const fromWebpack = o => Object.assign({ type: 'local', src: webpackOutput }, o);
  const dotMin = mode == 'dev' ? '' : '.min';

  const fromCdnjs = (lib, filename, manifest) => {
    const libC = lib.cdn || lib;
    const libN = lib.npm || lib;
    return {
      type: 'cdn',
      url: `https://cdnjs.cloudflare.com/ajax/libs/${libC}/${moduleVer(libN)}/${filename}`,
      integrity: { files: `node_modules/${libN}/dist/${filename}` },
      manifest,
    };
  };

  return {

    src: Path.resolve(__dirname, ".."),

    output: {
      dir: `dist/${mode}/serve`,
      name: name,
      manifest: false,
    },

    assets: {

      html: { type: 'local', src: 'shipreq/html', files: '**/*.html', outputName: '[path]/[basename]' },

      favicon: { type: 'local', src: 'shipreq/assets', files: 'favicon.ico', manifest: true },

      images: { type: 'local', src: 'shipreq/assets', files: '*.{svg,png}', manifest: CamelCase },

      webappClientPublic: [
        { type: 'external', path: sjsPath('public'), manifest: 'webappClientPublicJs' },
        'publicLibs',
      ],

      webappClientHome: [
        { type: 'external', path: sjsPath('home'), manifest: 'webappClientHomeJs' },
        'memberLibs',
      ],

      webappClientProject: [
        { type: 'external', path: sjsPath('project'), manifest: 'webappClientProjectJs' },
      ],

      webappClientWw: [
        { type: 'external', path: sjsPath('ww'), manifest: 'webappClientWwJs' },
        'vizJs',
      ],

      admin: fromWebpack({ files: 'admin.css' }),

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

      jquery: fromCdnjs('jquery', 'jquery.min.js', 'jqueryJs'),

      react: [
        fromCdnjs('react', mode == 'dev' ? 'react-with-addons.js' : 'react.min.js', 'reactJs'),
        fromCdnjs({cdn: 'react', npm: 'react-dom'}, `react-dom${dotMin}.js`, 'reactDomJs'),
      ],

      reactDomSvr: [
        'react',
        fromCdnjs({cdn: 'react', npm: 'react-dom'}, `react-dom-server${dotMin}.js`, 'reactDomServerJs'),
      ],

      katex: [
        fromCdnjs({cdn: 'KaTeX', npm: 'katex'}, `katex.min.js`, 'katexJs'),
        fromCdnjs({cdn: 'KaTeX', npm: 'katex'}, `katex.min.css`, 'katexCss'),
        // { type: 'local', src: 'node_modules/katex/dist', files: '*.min.{js,css}' },
        // { type: 'local', src: 'node_modules/katex/dist', files: 'fonts/**/*', transitive: true },
      ],

      vizJs: { type: 'local', files: 'vendor/viz.js', manifest: true },

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
    ],
  }
};

module.exports = makeConfig;
