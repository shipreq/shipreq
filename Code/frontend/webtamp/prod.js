const makeConfig = require('./common');

const sjsNames = {
  public : 'a',
  home   : 'h',
  project: 'p',
  ww     : 'w',
};

module.exports = makeConfig({
  mode: 'prod',
  name: '[hash:32].[ext]',
  sjsName: n => `${sjsNames[n]}.js`,
  htmlMinifyOptions: {
    removeComments: true,
    collapseWhitespace: true,
  },
});
