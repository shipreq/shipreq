const makeConfig = require('./common');

module.exports = makeConfig({

  mode: 'prod',

  name: '[hash:32].[ext]',

  // static resources all go in /s/ as is configured in web.xml
  staticDir: 's',

  htmlMinifyOptions: {
    removeComments: true,
    collapseWhitespace: true,
    minifyCSS: true,
  },
});
