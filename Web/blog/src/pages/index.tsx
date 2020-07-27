import { Link, graphql } from "gatsby"
import Layout from "../layouts/regular"
import React from "react"
import { pathForPost } from "../utils/routes"
import { Node as Post } from "../config/post"

export const pageQuery = graphql`
  query {
    allMdx(sort: { fields: [frontmatter___date], order: DESC }) {
      edges {
        node {
          ...PostNode
          excerpt
        }
      }
    }
  }
`

type Query = {
  data: {
    allMdx: {
      edges: [{
        node: Post & {
          excerpt: string
        }
      }]
    }
  }
}

export default function({ data }: Query) {
  const { edges: posts } = data.allMdx

  return (
    <Layout seo={{}}>

      <ul>
        {posts.map(({ node: post }) => (
          <li key={post.id}>
            <Link to={pathForPost(post)}>
              <h2>{post.frontmatter.title}</h2>
            </Link>
            <p>{post.excerpt}</p>
          </li>
        ))}
      </ul>

    </Layout>
  )
}
