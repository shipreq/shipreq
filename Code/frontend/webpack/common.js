const
  Path = require('path'),
  Webpack = require('webpack'),
  BundleAnalyzerPlugin = require('webpack-bundle-analyzer').BundleAnalyzerPlugin,
  MiniCssExtractPlugin = require('mini-css-extract-plugin'),
  NodeModules = Path.resolve(__dirname, '../node_modules');

const entryPoints = es => es.filter(e => !!e);

const config = ({ mode }) => ({

  entry: {
    'analytics'        : './shipreq/js/analytics.js',
    'member-lib-bundle': './shipreq/js/member-lib-bundle.js',
    'semantic'         : `./shipreq/semantic/require-${mode}`,
  },

  output: {
    path: `/tmp/shipreq.webpack.${mode}`,
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

  module: {
    rules: [

      // Transpile our analytics.js & libs autotrack & dom-utils out of ES6
      // See: https://github.com/googleanalytics/autotrack/issues/137
      {
        resource: { test: /node_modules\/(autotrack|dom-utils)\/|analytics\.js$/ },
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['@babel/preset-env'],
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
        test: /.css$/,
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
