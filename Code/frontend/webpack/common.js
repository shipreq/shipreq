const
  Path = require('path'),
  Webpack = require('webpack'),
  ExtractTextPlugin = require("extract-text-webpack-plugin"),
  NodeModules = Path.resolve(__dirname, '../node_modules');

const extractCss = new ExtractTextPlugin({ filename: '[name].css' });
const extractLess = new ExtractTextPlugin({ filename: '[name].css' });

const entryPoints = es => es.filter(e => !!e);

const config = ({ mode }) => ({

  entry: {

    semantic: `./shipreq/semantic/require-${mode}`,

    'member-lib-bundle': entryPoints([
      'expose-loader?ReactCollapse!react-collapse',
      'expose-loader?moment!moment',
      'expose-loader?autosize!autosize',
      'expose-loader?TextComplete!textcomplete/lib/textcomplete',
      'expose-loader?TextCompleteTA!textcomplete/lib/textarea',
    ]),

    admin: './shipreq/styles/admin.less',
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
    rules: [ //
      {
        test: /\.less$/,
        use: extractLess.extract({
          use: [{ loader: "css-loader" }, { loader: "less-loader" }],
          fallback: "style-loader",
        }),
      },
      {
        test: /.css$/,
        use: extractCss.extract({
          use: [
            { loader: 'css-loader', options: { importLoaders: 1, } },
            { loader: 'postcss-loader' },
          ],
          fallback: 'style-loader',
        }),
      },
      {
        test: /\.(ico|png|svg|eot|ttf|woff2?)$/,
        use: [{ loader: 'file-loader', options: { name: '[name].[ext]' } }],
      }
    ],
  },

  plugins: [
    new Webpack.NoEmitOnErrorsPlugin(),
    extractCss,
    extractLess,
  ],

  bail: true,
});

module.exports = config;
