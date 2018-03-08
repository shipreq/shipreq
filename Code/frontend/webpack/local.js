// This generates stuff needed locally (as opposed to assets that will be served.)

const
  Path = require('path'),
  Webpack = require('webpack'),
  NodeModules = Path.resolve(__dirname, '../node_modules');

const config = {

  // NOTE: airbnb-js-shims can be upgraded to airbnb-browser-shims if future shit crashes PhantomJS

  entry: {

    // Projects access this via symlink in src/test/resources
    'webapp-gen-deps': [
      'airbnb-js-shims',
      'expose-loader?React!react',
      'expose-loader?ReactDOM!react-dom',
      'expose-loader?ReactDOMServer!react-dom/server',
    ],

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
  },

  output: {
    path: Path.resolve(__dirname, '../dist/local'),
    filename: '[name].js',
    chunkFilename: 'chunk-[name]-[id].[ext]',
  },

  context: Path.resolve(__dirname, '..'),

  resolve: { modules: [NodeModules] },
  resolveLoader: { modules: [NodeModules] },

  plugins: [
    new Webpack.NoEmitOnErrorsPlugin(),

    new Webpack.LoaderOptionsPlugin({
      minimize: true,
    }),

    new Webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify('production')
    }),

    new Webpack.optimize.UglifyJsPlugin({
      compress: {
        screw_ie8: true,
        warnings: false,
      },
      output: {
        comments: false,
      },
      sourceMap: false,
    }),
  ],

  bail: true,
};

module.exports = config;
