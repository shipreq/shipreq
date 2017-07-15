// This is used from the webpack postcss plugin.
// It needs to live here and be renamed exactly thus for it is not explicitly declared.
module.exports = {
  plugins: [
    require('cssnano')({
      preset: 'default',
    }),
  ],
};
