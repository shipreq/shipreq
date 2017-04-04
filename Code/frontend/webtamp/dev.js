const makeConfig = require('./common');

module.exports = makeConfig({
  mode: 'dev',
  name: '[name].[ext]',
  sjsPath: n => `/shipreq-${n}.js`,
});
