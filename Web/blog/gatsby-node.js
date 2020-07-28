const post = require("./src/config/post")
const tag = require("./src/config/tag")

exports.createPages = async args => {
  await post.createPages(args)
  await tag.createPages(args)
}

exports.createSchemaCustomization = async ({ actions }) => {
  const { createTypes } = actions
  createTypes(post.graphTypeDefs)
}
