// This generates stuff needed locally for unit tests (as opposed to assets that will be served.)

const
  Path = require('path'),
  Webpack = require('webpack'),
  TerserPlugin = require('terser-webpack-plugin'),
  NodeModules = Path.resolve(__dirname, '../node_modules');

const config = {

  // Projects access these via symlinks in src/test/resources
  entry: {
    'webapp-base-test'  : './src/js/webapp-base-test.js',
    'webapp-member-test': './src/js/webapp-member-test.js',
    'webapp-client-test': './src/js/webapp-client-test.js',
  },

  output: {
    path: Path.resolve(__dirname, '../dist/test'),
    filename: '[name].js',
    chunkFilename: 'chunk-[name]-[id].[ext]',
    library: '',
    libraryTarget: 'window',
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

  module: {
    rules: [
      {
        use: {
          loader: 'babel-loader',
          options: {
            presets: [
              [
                '@babel/preset-env',
                {
                  targets: [
                    "maintained node versions",
                  ]
                }
              ],
            ],
          },
        },
      },
    ],
  },

  // Because React's act() issues warnings in production mode
  mode: 'development',

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
