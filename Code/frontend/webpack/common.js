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

    semantic: './shipreq/semantic/require',

    public: entryPoints([
      mode == 'dev' && 'expose-loader?Perf!react-addons-perf',
      'expose-loader?React!react',
      'expose-loader?ReactDOM!react-dom',
      'expose-loader?loadjs!loadjs',
    ]),

    member: entryPoints([
      mode == 'dev' && 'expose-loader?Perf!react-addons-perf',
      'expose-loader?React!react',
      'expose-loader?ReactDOM!react-dom',
      'expose-loader?ReactDOMServer!react-dom/server',
      'expose-loader?ReactCollapse!react-collapse',
      'jquery-textcomplete', // pulls in jquery
      'expose-loader?loadjs!loadjs',
      'expose-loader?moment!moment',
      'expose-loader?autosize!autosize',
    ]),

    admin: './shipreq/styles/admin.less',
  },

  externals: {
    jquery: 'jQuery',
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
        use: extractCss.extract({ fallback: 'style-loader', use: 'css-loader' }),
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
