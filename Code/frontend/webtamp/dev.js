const makeConfig = require('./common');

module.exports = makeConfig({
  mode: 'dev',
  name: '[name]-[hash:8].[ext]',
  sjsPath: n => `/shipreq-${n}.js`,
});
