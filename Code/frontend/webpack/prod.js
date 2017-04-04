const
  Webpack = require('webpack'),
  WebpackMerge = require('webpack-merge'),
  common = require('./common');

const ctx = {
  mode: 'prod',
}

const config = WebpackMerge(common(ctx), {

  plugins: [

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

});

module.exports = config;
