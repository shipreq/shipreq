const
  Path = require('path'),
  Webpack = require('webpack'),
  BundleAnalyzerPlugin = require('webpack-bundle-analyzer').BundleAnalyzerPlugin,
  LodashModuleReplacementPlugin = require('lodash-webpack-plugin'),
  MiniCssExtractPlugin = require('mini-css-extract-plugin'),
  NodeModules = Path.resolve(__dirname, '../node_modules');

const entryPoints = es => es.filter(e => !!e);

const config = ({ mode }) => ({

  entry: {

    semantic: `./shipreq/semantic/require-${mode}`,

    'member-lib-bundle': entryPoints([
      'expose-loader?ReactCollapse!react-collapse',
      'expose-loader?moment!moment/min/moment.min.js',
      'expose-loader?autosize!autosize',
      'expose-loader?clipboard!clipboard-polyfill',
      'expose-loader?scrollIntoView!scroll-into-view-if-needed',
      'expose-loader?TextComplete!textcomplete/lib/textcomplete',
      'expose-loader?TextCompleteTA!textcomplete/lib/textarea',
      'expose-loader?GithubPicker!react-color/lib/components/github/Github',
      'expose-loader?ChromePicker!react-color/lib/components/chrome/Chrome',
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

    // react-color uses:
    //   - import each from 'lodash/each'
    //   - import merge from 'lodash/merge'
    //   - import map from 'lodash/map'
    //   - import throttle from 'lodash/throttle'
    //   - import debounce from 'lodash/debounce'
    new LodashModuleReplacementPlugin({
      shorthands  : false, // Iteratee shorthands for _.property, _.matches, & _.matchesProperty.
      cloning     : false, // Support “clone” methods & cloning source objects.
      currying    : false, // Support “curry” methods.
      caching     : false, // Caches for methods like _.cloneDeep, _.isEqual, & _.uniq.
      collections : false, // Support objects in “Collection” methods.
      exotics     : false, // Support objects like buffers, maps, sets, symbols, typed arrays, etc.
      guards      : false, // Guards for host objects, sparse arrays, & other edge cases.
      metadata    : false, // Metadata to reduce wrapping of bound, curried, & partially applied functions. (requires currying)
      deburring   : false, // Support deburring letters.
      unicode     : false, // Support Unicode symbols.
      chaining    : false, // Components to support chain sequences.
      memoizing   : false, // Support _.memoize & memoization.
      coercions   : false, // Support for coercing values to integers, numbers, & strings.
      flattening  : false, // Support “flatten” methods & flattening rest arguments.
      paths       : false, // Deep property path support for methods like _.get, _.has, & _.set.
      placeholders: false, // Argument placeholder support for “bind”, “curry”, & “partial” methods. (requires currying)
    }),

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
