const makeConfig = require('./common');

module.exports = makeConfig({
  mode: 'prod',
  name: '[hash:32].[ext]',
  sjsPath: n => `/${n[0]}.js`,
  htmlMinifyOptions: {
    removeComments: true,
    collapseWhitespace: true,
  },
});
