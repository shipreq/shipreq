const
  Path = require('path'),
  BundleAnalyzerPlugin = require('webpack-bundle-analyzer').BundleAnalyzerPlugin,
  MiniCssExtractPlugin = require('mini-css-extract-plugin'),
  NodeModules = Path.resolve(__dirname, '../node_modules');

const config = ({ mode }) => ({

  entry: {
    'analytics'        : './src/js/analytics.js',
    'member-lib-bundle': './src/js/member-lib-bundle.js',
    'semantic'         : `./src/semantic/require-${mode}`,
  },

  output: {
    path: `/tmp/shipreq.webpack.${mode}`,
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

      {
        test: /\.less$/,
        use: [
          MiniCssExtractPlugin.loader,
          'css-loader',
          'less-loader',
        ],
      },

      {
        test: /\.css$/,
        use: [
          MiniCssExtractPlugin.loader,
          'css-loader',
          'postcss-loader',
        ],
      },

      {
        test: /\.(ico|png|svg|eot|ttf|woff2?)$/,
        use: [{ loader: 'file-loader', options: { name: '[name].[ext]' } }],
      }
    ],
  },

  plugins: [
    new MiniCssExtractPlugin({ filename: '[name].css' }),

    new BundleAnalyzerPlugin({
      analyzerMode: 'static',
      defaultSizes: 'parsed',
      openAnalyzer: false,
      generateStatsFile: true,
      statsFilename: `analysis/${mode}.json`,
      reportFilename: `analysis/${mode}.html`,
    }),
  ],

  bail: true,
});

module.exports = config;
