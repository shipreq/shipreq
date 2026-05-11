// This generates stuff needed locally for unit tests (as opposed to assets that will be served),
// but unlike ./test.js these artifacts must be loaded by node's `require` to work.

const
  Path = require('path'),
  NodeModules = Path.resolve(__dirname, '../node_modules');

const config = {

  entry: {
    'fake-indexeddb': './src/js/fake-indexeddb.js',
  },

  target: 'node',

  output: {
    path: Path.resolve(__dirname, '../dist/test-node'),
    filename: '[name].js',
    library: '',
    libraryTarget: 'global',
  },

  context: Path.resolve(__dirname, '..'),

  resolve: {
    modules: [
      NodeModules,
      'node_modules',
    ],
  },
  resolveLoader: {
    modules: [
      NodeModules,
    ],
  },

  // Using 'production' here breaks fake-indexeddb somehow.
  // Running the code in https://github.com/dumbmatter/fakeIndexedDB#use from Scala.JS prints:
  //
  //     From index: { title: 'Quarry Memories', author: 'Fred', isbn: 123456 }
  //     From cursor: undefined
  //     From cursor: undefined
  //     All done!
  //
  // instead of the expected
  //
  //    From index: { title: 'Quarry Memories', author: 'Fred', isbn: 123456 }
  //    From cursor: { title: 'Water Buffaloes', author: 'Fred', isbn: 234567 }
  //    From cursor: { title: 'Bedrock Nights', author: 'Barney', isbn: 345678 }
  //    All done!
  //
  mode: 'development',

  performance: {
    hints: false
  },

  bail: true,
};

module.exports = config;
