const
  Webpack = require('webpack'),
  WebpackMerge = require('webpack-merge'),
  common = require('./runtime');

const ctx = {
  mode: 'dev',
}

const config = WebpackMerge(common(ctx), {
  mode: 'development',
});

// console.log("CONFIG: ", config)

module.exports = config;
