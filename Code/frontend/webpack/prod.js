const
  Webpack = require('webpack'),
  WebpackMerge = require('webpack-merge'),
  TerserPlugin = require('terser-webpack-plugin'),
  common = require('./runtime');

const ctx = {
  mode: 'prod',
}

const config = WebpackMerge(common(ctx), {

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

});

module.exports = config;
