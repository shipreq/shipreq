import { GatsbyNode } from "gatsby"
import { pathForPage } from "../utils/routes"
import * as path from "path"

export type Node = {
  id: string
  frontmatter: {
    title: string
    slug : string
  }
}

export type PageContext = {
  id: string
}

const templatePath = path.resolve(`${__dirname}/../templates/page.tsx`)

export const createPages: GatsbyNode["createPages"] = async ({ graphql, actions, reporter }) => {
  const { createPage } = actions

  type Query = {
    allMdx: {
      edges: [{
        node: Node
      }]
    }
  }

  const result = await graphql<Query>(`
    query {
      allMdx(
        sort: { fields: [frontmatter___slug], order: DESC },
        filter: {fileAbsolutePath: {glob: "**/pages/*"}}
      ) {
        edges {
          node {
            id
            frontmatter {
              title
              slug
            }
          }
        }
      }
    }
  `)

  if (result.errors) {
    reporter.panicOnBuild('🚨  ERROR: Loading "createPages" query')
    return
  }

  if (result.data) {
    const nodes = result.data.allMdx.edges
    nodes.forEach(({ node }) => {

      const context: PageContext = {
        id: node.id,
      }

      createPage({
        path     : pathForPage(node),
        component: templatePath,
        context,
      })
    })
  }
}
