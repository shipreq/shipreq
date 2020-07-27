import { GatsbyNode } from "gatsby"
import { pathForTag } from "../utils/routes"
import * as path from "path"

const templatePath = path.resolve(`${__dirname}/../pages/tag.tsx`)

export const createPages: GatsbyNode["createPages"] = async ({ graphql, actions, reporter }) => {
  const { createPage } = actions

  type Query = {
    allMdx: {
      tags: [{
        name: string
      }]
    }
  }

  const result = await graphql<Query>(`
    query {
      allMdx {
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

  if (result.data) {
    result.data.allMdx.tags.forEach(tag => {
      createPage({
        path     : pathForTag(tag.name),
        component: templatePath,
        context  : { tag: tag.name },
      })
    })
  }
}
