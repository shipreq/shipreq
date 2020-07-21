const path = require("path")
const postComponentPath = path.resolve(`./src/components/post.tsx`);

function postPath(node) {
  return `/post/${node.frontmatter.slug}`;
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
      }
    }
  `)

  if (result.errors) {
    reporter.panicOnBuild('🚨  ERROR: Loading "createPages" query')
  }

  result.data.allMdx.edges.forEach(({ node }, index) => {
    createPage({
      path: postPath(node),
      component: postComponentPath,
      context: { id: node.id },
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
