const
  Webpack = require('webpack'),
  WebpackMerge = require('webpack-merge'),
  common = require('./common');

const ctx = {
  mode: 'dev',
}

const config = WebpackMerge(common(ctx), {

  plugins: [

    new Webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify('development')
    }),

  ],

});

module.exports = config;
