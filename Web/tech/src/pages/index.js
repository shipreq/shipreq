import React from "react"
import { Link, graphql } from "gatsby"
import ShipreqBanner from "../components/shipreqBanner"
import SEO from "../components/seo"

export default function({ data }) {
  const { edges: posts } = data.allMdx
  const md = data.site.siteMetadata

  return (
    <div>

      <SEO
        desc  = {md.description}
        title = {md.title}
        path  = ""
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
