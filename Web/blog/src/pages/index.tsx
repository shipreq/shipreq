import { Link, graphql } from "gatsby"
import React from "react"
import SEO from "../components/seo"
import ShipreqBanner from "../components/shipreqBanner"

export const pageQuery = graphql`
  query {
    site {
      siteMetadata {
        description
        title
      }
    }
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
    site: {
      siteMetadata: {
        description: string
        title: string
      }
    }
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
  const md = data.site.siteMetadata

  return (
    <div>

      <SEO
        article = {false}
        desc    = {md.description}
        path    = ""
        title   = {md.title}
      />

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
