const makeConfig = require('./common');

module.exports = makeConfig({

  mode: 'dev',

  name: '[name].[ext]',

  // use a different dir than is configured in web.xml to avoid dev caching
  // (filenames aren't hashes here)
  staticDir: 'assets',

});
