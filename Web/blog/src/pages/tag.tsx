import { pathForTag } from "../utils/routes"
import { graphql } from "gatsby"
import { Node as Post } from "../config/post"
import Layout from "../layouts/regular"
import React from "react"
import Tag from "../components/tag"
import PostList from "../components/post-list"
import styled from "styled-components"

export const pageQuery = graphql`
  query TagPageQuery($tag: String) {
    allMdx(sort: {fields: [frontmatter___date], order: DESC}, filter: {frontmatter: {tags: {in: [$tag]}}}) {
      edges {
        node {
          ...PostNode
          excerpt
        }
      }
    }
  }
`

type Input = {
  data: {
    allMdx: {
      edges: [{
        node: Post & {
          excerpt: string
        }
      }]
    }
  }
  pageContext: {
    tag: string
  }
}

const Header = styled.h1`
  color: #011b31;
  font-weight: bold;
  letter-spacing: 0.1ex;
  border-bottom: solid 1px #011b3133;
  padding-bottom: 0.1em;
  margin-bottom: 1em;
`

export default function(input: Input) {
  const { edges: posts } = input.data.allMdx
  const tag = input.pageContext.tag

  const seo = {
    subtitle: "#" + tag,
    path    : pathForTag(tag),
  }

  return (
    <Layout seo={seo}>

      <Header><Tag name={tag} notAsLink /></Header>

      <PostList posts={posts.map(n => n.node)} />

    </Layout>
  )
}
