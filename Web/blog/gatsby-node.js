const kebabCase = require("lodash/kebabCase")
const path = require("path")

function postPath(node) {
  return `/post/${node.frontmatter.slug.replace(/^\/+/, '')}`;
}

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
      path: postPath(node),
      component: postTemplatePath,
      context: { id: node.id },
    })
  })

  // Create pages for tags
  const tagsTemplatePath = path.resolve(`./src/templates/tags.tsx`);
  result.data.allMdx.tags.forEach(tag => {
    createPage({
      path: `/tag/${kebabCase(tag.name)}`,
      component: tagsTemplatePath,
      context: { tag: tag.name },
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
      value: postPath(node),
    })
  }
};
