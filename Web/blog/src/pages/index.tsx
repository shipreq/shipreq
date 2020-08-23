import { graphql } from "gatsby"
import { Node as Post } from "../config/posts"
import Layout from "../layouts/regular"
import PostList from "../components/post-list"
import React from "react"

export const pageQuery = graphql`
  query {
    allMdx(
      sort: { fields: [frontmatter___date], order: DESC }
      filter: {fileAbsolutePath: {glob: "**/posts/*"}}
    ) {
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

      <PostList posts={posts.map(n => n.node)} />

    </Layout>
  )
}
