import React from "react";
import { Link, graphql } from "gatsby";
import ShipreqBanner from "../images/shipreq-banner.inline.svg";

const Index = ({ data }) => {
  const { edges: posts } = data.allMdx

  return (
    <div>

      <ShipreqBanner />

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

export default Index;
