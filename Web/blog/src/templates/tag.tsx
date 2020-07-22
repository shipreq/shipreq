import { Link, graphql } from "gatsby"
import { linkToTagIndex } from "../utils/routes"
import React from "react"

export const pageQuery = graphql`
  query($tag: String) {
    allMdx(
      limit: 2000
      sort: { fields: [frontmatter___date], order: DESC }
      filter: { frontmatter: { tags: { in: [$tag] } } }
    ) {
      totalCount
      edges {
        node {
          fields {
            path
          }
          frontmatter {
            title
          }
        }
      }
    }
  }
`

type Props = {
  pageContext: {
    tag: string
  }
  data: {
    allMdx: {
      totalCount: number
      edges: [{
        node: {
          fields: {
            path: string
          }
          frontmatter: {
            title: string
          }
        }
      }]
    }
  }
}

export default function({ pageContext, data }: Props) {
  const { tag } = pageContext
  const { edges, totalCount } = data.allMdx

  const tagHeader = `${totalCount} post${
    totalCount === 1 ? "" : "s"
  } tagged with "${tag}"`

  return (
    <div>
      <h1>{tagHeader}</h1>
      <ul>
        {edges.map(({ node }) => {
          const { path } = node.fields
          const { title } = node.frontmatter
          return (
            <li key={path}>
              <Link to={path}>{title}</Link>
            </li>
          )
        })}
      </ul>
      {linkToTagIndex}
    </div>
  )
}
