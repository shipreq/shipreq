const
  CamelCase = require('camelcase'),
  Path = require('path'),
  Webtamp = require('webtamp');
  // Webtamp = require('/home/golly/projects/webtamp/src/main');


const moduleVer = n => require(`../node_modules/${n}/package.json`).version;

const makeConfig = ({ mode, name }) => {

  const webpackOutput = `/tmp/shipreq.webpack.${mode}`;
  const fromWebpack = o => Object.assign({ type: 'local', src: webpackOutput }, o);

  return {

    src: Path.resolve(__dirname, ".."),

    output: {
      dir: `dist/${mode}`,
      name: name,
      manifest: false,
    },

    assets: {
      images: { type: 'local', src: 'shipreq', files: '*.{svg,ico,png}', manifest: CamelCase },
      publicCss: fromWebpack({ files: 'public.css' }),
      sirCss: fromWebpack({ files: 'sir.css' }),
      webappClientWw: [
        { type: 'external', path: '/ww.js', manifest: 'webappClientWwJs' },
        'vizJs',
      ],
      webappClientHome: 'member',
      webappClientProject: 'member',
    },

    optional: {
      semantic: [
        fromWebpack({ files: 'semantic.*' }),
        fromWebpack({ files: 'icons.*', outputName: '[path]/[basename]' }),
      ],

      memberBundle: [
        fromWebpack({ files: 'member.js' }),
        'jqueryCdn',
      ],

      member: [
        'memberBundle',
        'semantic',
        'katex',
      ],

      jqueryCdn: {
        type: 'cdn',
        url: `https://cdnjs.cloudflare.com/ajax/libs/jquery/${moduleVer('jquery')}/jquery.min.js`,
        integrity: { files: 'node_modules/jquery/dist/jquery.min.js' },
      },

      katex: [
        { type: 'local', src: 'node_modules/katex/dist', files: '*.min.js' },
        { type: 'local', src: 'node_modules/katex/dist', files: 'fonts/**/*', outputName: '[path]/[basename]' },
      ],

      vizJs: { type: 'local', files: 'vendor/viz.js', manifest: true },
    },

    plugins: [
      Webtamp.plugins.Inline.data(i => /\.(svg|png)$/.test(i.dest) && i.size() < 4096),
      Webtamp.plugins.ScalaManifest({ object: "shipreq.webapp.base.AssetManifest", outputPath: 'scala' }),
    ],
  }
};

module.exports = makeConfig;
