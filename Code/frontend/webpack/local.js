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
      'airbnb-js-shims',
      'expose-loader?React!react',
      'expose-loader?ReactDOM!react-dom',
      'expose-loader?ReactTestUtils!react-dom/test-utils',
    ],

    // Projects access this via symlink in src/test/resources
    'webapp-client-test': [
      'airbnb-js-shims',
      'expose-loader?React!react',
      'expose-loader?ReactDOM!react-dom',
      'expose-loader?ReactDOMServer!react-dom/server',
      'expose-loader?ReactTestUtils!react-dom/test-utils',
      'expose-loader?ReactCollapse!react-collapse',
      'expose-loader?moment!moment',
      'expose-loader?autosize!autosize',
      'expose-loader?TextComplete!textcomplete/lib/textcomplete',
      'expose-loader?TextCompleteTA!textcomplete/lib/textarea',
      'expose-loader?$!expose-loader?jQuery!jquery', // for Semantic UI -- must precede it! order in this array matters
      './semantic/dist/semantic.min', //.js
    ],

    // webappSsrJvm accesses this via symlink in src/main/resources
    'webapp-ssr-deps': [
      'expose-loader?React!react',
      'expose-loader?ReactDOMServer!react-dom/server',
      // 'expose-loader?React!react/umd/react.production.min.js',
      // 'expose-loader?ReactDOMServer!react-dom/umd/react-dom-server.browser.production.min.js',
    ],
  },

  output: {
    path: Path.resolve(__dirname, '../dist/local'),
    filename: '[name].js',
    chunkFilename: 'chunk-[name]-[id].[ext]',
  },

  context: Path.resolve(__dirname, '..'),

  resolve: { modules: [NodeModules] },
  resolveLoader: { modules: [NodeModules] },

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
