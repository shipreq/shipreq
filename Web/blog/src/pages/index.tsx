import { Link, graphql } from "gatsby"
import Analytics from "../components/analytics"
import React from "react"
import SEO from "../components/seo"
import ShipreqBanner from "../components/shipreq-banner"

export const pageQuery = graphql`
  query {
    allMdx {
      edges {
        node {
          id
          excerpt
          frontmatter {
            title
          }
          fields {
            path
          }
        }
      }
    }
  }
`

type Query = {
  data: {
    allMdx: {
      edges: [{
        node: {
          id: string
          excerpt: string
          frontmatter: {
            title: string
          }
          fields: {
            path: string
          }
        }
      }]
    }
  }
}

export default function({ data }: Query) {
  const { edges: posts } = data.allMdx

  return (
    <div>

      <Analytics />
      <SEO path = "" />

      <ShipreqBanner height={100} />

      <h1>Awesome MDX Blog</h1>

      <ul>
        {posts.map(({ node: post }) => (
          <li key={post.id}>
            <Link to={post.fields.path}>
              <h2>{post.frontmatter.title}</h2>
            </Link>
            <p>{post.excerpt}</p>
          </li>
        ))}
      </ul>
    </div>
  )
}
