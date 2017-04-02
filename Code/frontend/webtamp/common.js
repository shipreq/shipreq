const
  CamelCase = require('camelcase'),
  Path = require('path'),
  Webtamp = require('webtamp');
  // Webtamp = require('/home/golly/projects/webtamp/src/main');


const moduleVer = n => require(`../node_modules/${n}/package.json`).version;

const makeConfig = ({ mode, name, htmlMinifyOptions }) => {

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

      images: { type: 'local', src: 'shipreq/assets', files: '*.{svg,ico,png}', manifest: CamelCase },

      public: [
        fromWebpack({ files: 'public.css' }),
        'jquery',
      ],

      sir: fromWebpack({ files: 'sir.css' }),

      webappClientWw: [
        { type: 'external', path: '/ww.js', manifest: 'webappClientWwJs' },
        'vizJs',
      ],

      webappClientHome: [
        { type: 'external', path: '/h.js', manifest: 'webappClientHomeJs' },
        'member',
      ],

      webappClientProject: [
        { type: 'external', path: '/p.js', manifest: 'webappClientProjectJs' },
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
        { type: 'local', src: 'node_modules/katex/dist', files: '*.min.js' },
        { type: 'local', src: 'node_modules/katex/dist', files: 'fonts/**/*', transitive: true },
      ],

      vizJs: { type: 'local', files: 'vendor/viz.js', manifest: true },
    },

    plugins: [
      Webtamp.plugins.Inline.data(i => /\.(svg|png)$/.test(i.dest) && i.size() < 4096),
      Webtamp.plugins.Html.replace(),
      htmlMinifyOptions && Webtamp.plugins.Html.minify({options: htmlMinifyOptions}),
      Webtamp.plugins.ScalaManifest({ object: "shipreq.webapp.base.AssetManifest", outputPath: '../scala' }),
    ],
  }
};

module.exports = makeConfig;
