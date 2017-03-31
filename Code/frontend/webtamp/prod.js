const makeConfig = require('./common');

module.exports = makeConfig({
  mode: 'prod',
  name: '[hash:32].[ext]',
});
