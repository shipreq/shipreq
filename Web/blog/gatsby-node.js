const pages = require("./src/config/pages")
const posts = require("./src/config/posts")
const tags = require("./src/config/tags")

exports.createPages = async args => {
  await pages.createPages(args)
  await posts.createPages(args)
  await tags.createPages(args)
}

exports.createSchemaCustomization = async ({ actions }) => {
  const { createTypes } = actions
  createTypes(posts.graphTypeDefs)
}

// const path = require("path")
// exports.onCreateWebpackConfig = ({ actions }) => {
//   actions.setWebpackConfig({
//     resolve: {
//       modules: [path.resolve(__dirname, 'src'), 'node_modules'],
//     },
//   })
// }
