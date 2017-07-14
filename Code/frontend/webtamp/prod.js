const makeConfig = require('./common');

const sjsPaths = {
  public : 'a',
  home   : 'h',
  project: 'p',
  ww     : 'w',
};

module.exports = makeConfig({
  mode: 'prod',
  name: '[hash:32].[ext]',
  sjsPath: n => `/${sjsPaths[n]}.js`,
  htmlMinifyOptions: {
    removeComments: true,
    collapseWhitespace: true,
  },
});
