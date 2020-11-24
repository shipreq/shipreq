const
  Path = require('path'),
  BundleAnalyzerPlugin = require('webpack-bundle-analyzer').BundleAnalyzerPlugin,
  MiniCssExtractPlugin = require('mini-css-extract-plugin'),
  NodeModules = Path.resolve(__dirname, '../node_modules');

const config = ({ mode }) => ({

  entry: {
    'ww': './src/js/ww.js',
  },

  target: 'webworker',

  output: {
    path: `/tmp/shipreq.webpack.${mode}.ww`,
    filename: '[name].js',
    chunkFilename: 'chunk-[name]-[id].[ext]',
    library: '',
    libraryTarget: 'self',
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
        test: /\.js$/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: [
              [
                '@babel/preset-env',
                {
                  targets: [
                    "last 2 Chrome versions",
                    "last 2 Firefox versions",
                    "last 2 Edge versions",
                    "last 2 iOS versions",
                    "last 2 Safari versions",
                  ]
                }
              ],
            ],
          },
        },
      },

    ],
  },

  plugins: [

    new BundleAnalyzerPlugin({
      analyzerMode: 'static',
      defaultSizes: 'parsed',
      openAnalyzer: false,
      generateStatsFile: true,
      statsFilename: `analysis/ww-${mode}.json`,
      reportFilename: `analysis/ww-${mode}.html`,
    }),
  ],

  bail: true,
});

module.exports = config;
