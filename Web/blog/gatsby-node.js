const path = require("path")
const routes = require("./src/utils/routes")

// =================================================================================================
exports.createPages = async ({ graphql, actions, reporter }) => {
  const { createPage } = actions

  const result = await graphql(`
    query {
      allMdx {
        edges {
          node {
            id
            frontmatter {
              slug
            }
          }
        }
        tags: group(field: frontmatter___tags) {
          name: fieldValue
        }
      }
    }
  `)

  if (result.errors) {
    reporter.panicOnBuild('🚨  ERROR: Loading "createPages" query')
    return
  }

  // Create pages for posts
  const postTemplatePath = path.resolve(`./src/templates/post.tsx`);
  result.data.allMdx.edges.forEach(({ node }, index) => {
    createPage({
      path     : routes.pathForPost({node}),
      component: postTemplatePath,
      context  : { id: node.id },
    })
  })

  // Create pages for tags
  const tagsTemplatePath = path.resolve(`./src/templates/tag.tsx`);
  result.data.allMdx.tags.forEach(tag => {
    createPage({
      path     : routes.pathForTag(tag.name),
      component: tagsTemplatePath,
      context  : { tag: tag.name },
    })
  })

};

// =================================================================================================
exports.onCreateNode = ({ node, actions, getNode }) => {
  const { createNodeField } = actions

  if (node.internal.type === `Mdx`) {
    createNodeField({
      node,
      name: `path`,
      value: routes.pathForPost({node}),
    })
  }
};
