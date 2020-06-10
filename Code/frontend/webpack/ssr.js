// This generates stuff needed locally (as opposed to assets that will be served.)

const
  Path = require('path'),
  Webpack = require('webpack'),
  TerserPlugin = require('terser-webpack-plugin'),
  NodeModules = Path.resolve(__dirname, '../node_modules');

const config = {

  entry: {
    // webappSsrJvm accesses this via symlink in src/main/resources
    'webapp-ssr-deps': './src/js/webapp-ssr-deps.js',
  },

  output: {
    path: Path.resolve(__dirname, '../dist/ssr'),
    filename: '[name].js',
    chunkFilename: 'chunk-[name]-[id].[ext]',
    library: '',
    libraryTarget: 'this',
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
                  targets: "node 12.15.0" // = Graal VM 20.1
                }
              ],
            ],
          },
        },
      },
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
