const makeConfig = require('./common');

module.exports = makeConfig({
  mode: 'dev',
  name: '[name].[ext]',
  sjsName: n => `shipreq-${n}.js`,
});
