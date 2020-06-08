// This generates stuff needed locally (as opposed to assets that will be served.)

const
  Path = require('path'),
  Webpack = require('webpack'),
  TerserPlugin = require('terser-webpack-plugin'),
  NodeModules = Path.resolve(__dirname, '../node_modules');

const config = {

  // NOTE: airbnb-js-shims can be upgraded to airbnb-browser-shims if future shit crashes PhantomJS

  entry: {

    // Projects access this via symlink in src/test/resources
    'webapp-base-test': [
      './shipreq/js/webapp-base-test.js',
      'expose-loader?$!expose-loader?jQuery!jquery', // for Semantic UI -- must precede it! order in this array matters
      './semantic/dist/semantic.min', //.js
    ],

    // Projects access this via symlink in src/test/resources
    'webapp-client-test': [
      './shipreq/js/webapp-client-test.js',
      'expose-loader?$!expose-loader?jQuery!jquery', // for Semantic UI -- must precede it! order in this array matters
      './semantic/dist/semantic.min', //.js
    ],

    // webappSsrJvm accesses this via symlink in src/main/resources
    'webapp-ssr-deps': './shipreq/js/webapp-ssr-deps.js',
  },

  output: {
    path: Path.resolve(__dirname, '../dist/local'),
    filename: '[name].js',
    chunkFilename: 'chunk-[name]-[id].[ext]',
  },

  context: Path.resolve(__dirname, '..'),

  resolve: {
    modules: [
      NodeModules,
      'node_modules',
    ],
  },
  resolveLoader: {
    modules: [
      NodeModules,
    ],
  },

  mode: 'production',

  performance: {
    hints: false
  },

  optimization: {
    minimizer: [new TerserPlugin({
      cache: true,
      parallel: true,
      terserOptions: {
        output: {
          comments: false,
        }
      },
    })]
  },

  plugins: [
    new Webpack.LoaderOptionsPlugin({
      minimize: true,
    }),
  ],

  bail: true,
};

module.exports = config;
