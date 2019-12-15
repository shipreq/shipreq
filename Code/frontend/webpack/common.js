const
  Path = require('path'),
  Webpack = require('webpack'),
  MiniCssExtractPlugin = require('mini-css-extract-plugin'),
  NodeModules = Path.resolve(__dirname, '../node_modules');

const entryPoints = es => es.filter(e => !!e);

const config = ({ mode }) => ({

  entry: {

    semantic: `./shipreq/semantic/require-${mode}`,

    'member-lib-bundle': entryPoints([
      'expose-loader?ReactCollapse!react-collapse',
      'expose-loader?moment!moment',
      'expose-loader?autosize!autosize',
      'expose-loader?clipboard!clipboard-polyfill',
      'expose-loader?TextComplete!textcomplete/lib/textcomplete',
      'expose-loader?TextCompleteTA!textcomplete/lib/textarea',
    ]),

    analytics: './shipreq/js/analytics.js',
  },

  output: {
    path: `/tmp/shipreq.webpack.${mode}`,
    filename: '[name].js',
    chunkFilename: 'chunk-[name]-[id].[ext]',
  },

  context: Path.resolve(__dirname, '..'),

  resolve: { modules: [NodeModules] },
  resolveLoader: { modules: [NodeModules] },

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
  ],

  bail: true,
});

module.exports = config;
