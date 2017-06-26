const
  CamelCase = require('camelcase'),
  Path = require('path'),
  Webtamp = require(process.env.WEBTAMP ? `${process.env.WEBTAMP}/src/main` : 'webtamp');


const moduleVer = n => require(`../node_modules/${n}/package.json`).version;

const fixLinksInLiftTemplates = i =>
  /<head[ >]/.test(i.content()) ? t => t :
  tag => tag.replace(/^(<link )/, '$1data-lift="head" ');

const makeConfig = ({ mode, name, sjsPath, htmlMinifyOptions }) => {

  const webpackOutput = `/tmp/shipreq.webpack.${mode}`;
  const fromWebpack = o => Object.assign({ type: 'local', src: webpackOutput }, o);

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

      public: [
        fromWebpack({ files: 'public.css' }),
        'jquery',
      ],

      admin: fromWebpack({ files: 'admin.css' }),

      webappClientWw: [
        { type: 'external', path: sjsPath('ww'), manifest: 'webappClientWwJs' },
        'vizJs',
      ],

      webappClientPublic: [
        { type: 'external', path: sjsPath('public'), manifest: 'webappClientPublicJs' },
      ],

      webappClientHome: [
        { type: 'external', path: sjsPath('home'), manifest: 'webappClientHomeJs' },
        'member',
      ],

      webappClientProject: [
        { type: 'external', path: sjsPath('project'), manifest: 'webappClientProjectJs' },
        'member',
      ],
    },

    optional: {
      semantic: [
        fromWebpack({ files: 'semantic.*' }),
        fromWebpack({ files: 'icons.*', transitive: true }),
      ],

      memberBundle: [
        fromWebpack({ files: 'member.js' }),
        'jquery',
      ],

      member: [
        'memberBundle',
        'semantic',
        'katex',
      ],

      jquery: {
        type: 'cdn',
        url: `https://cdnjs.cloudflare.com/ajax/libs/jquery/${moduleVer('jquery')}/jquery.min.js`,
        integrity: { files: 'node_modules/jquery/dist/jquery.min.js' },
      },

      katex: [
        { type: 'local', src: 'node_modules/katex/dist', files: '*.min.{js,css}' },
        { type: 'local', src: 'node_modules/katex/dist', files: 'fonts/**/*', transitive: true },
      ],

      vizJs: { type: 'local', files: 'vendor/viz.js', manifest: true },
    },

    plugins: [
      Webtamp.plugins.Inline.data(i => /\.(svg|png)$/.test(i.dest) && i.size() < 4096),
      Webtamp.plugins.Html.replace({ modTag: fixLinksInLiftTemplates }),
      htmlMinifyOptions && Webtamp.plugins.Html.minify({ options: htmlMinifyOptions }),
      Webtamp.plugins.ScalaManifest({ object: "shipreq.webapp.base.AssetManifest", outputPath: '../scala' }),
    ],
  }
};

module.exports = makeConfig;
