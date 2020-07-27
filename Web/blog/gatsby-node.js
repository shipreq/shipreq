const post = require("./src/config/post")
const tag = require("./src/config/tag")

exports.createPages = async input => {
  await post.createPages(input)
  await tag.createPages(input)
}
