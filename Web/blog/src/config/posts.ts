import { GatsbyNode } from "gatsby"
import * as path from "path"
import { pathForPost } from "../utils/routes"

export type Node = {
  id: string
  frontmatter: {
    title   : string
    slug    : string
    date    : string
    desc    : string
    twitter?: string
    reddit? : string
    hn?     : string
    tags    : Array<string>
  }
}

export type PageContext = {
  id   : string
  older: Node | null
  newer: Node | null
}

const templatePath = path.resolve(`${__dirname}/../templates/post.tsx`)

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
        sort: { fields: [frontmatter___date], order: DESC },
        filter: {fileAbsolutePath: {glob: "**/posts/*"}}
      ) {
        edges {
          node {
            id
            frontmatter {
              title
              slug
              date
              desc
              twitter
              reddit
              hn
              tags
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
    const last = nodes.length - 1
    nodes.forEach(({ node }, index) => {
      const older = index === last ? null : nodes[index + 1].node
      const newer = index === 0    ? null : nodes[index - 1].node

      const context: PageContext = {
        id: node.id,
        older,
        newer,
      }

      createPage({
        path     : pathForPost(node),
        component: templatePath,
        context,
      })
    })
  }
}

export const graphTypeDefs = `
  type Mdx implements Node {
    frontmatter: MdxFrontmatter
  }
  type MdxFrontmatter {
    title  : String!
    slug   : String!
    date   : String!
    desc   : String!
    tags   : [String]!
    twitter: String
    reddit : String
    hn     : String
  }
`
