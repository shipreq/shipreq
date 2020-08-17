const posts = require("./src/config/posts")
const tags = require("./src/config/tags")

exports.createPages = async args => {
  await posts.createPages(args)
  await tags.createPages(args)
}

exports.createSchemaCustomization = async ({ actions }) => {
  const { createTypes } = actions
  createTypes(posts.graphTypeDefs)
}
