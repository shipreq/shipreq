import { graphql } from "gatsby"
import { Node as Post } from "../config/posts"
import { pathForTag } from "../utils/routes"
import Layout from "../layouts/regular"
import PostList from "../components/post-list"
import R from "../utils/responsive"
import React from "react"
import styled from "styled-components"
import Tag from "../components/tag"

export const pageQuery = graphql`
  query TagPageQuery($tag: String) {
    allMdx(
      sort: {fields: [frontmatter___date], order: DESC},
      filter: {fileAbsolutePath: {glob: "**/posts/*"}, frontmatter: {tags: {in: [$tag]}}}
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

const Header = styled.header`
  margin-bottom: 2.8em;
  ${R.phone`
    margin-top: 1em;
  `}
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

      <Header>
        <h1 className="tagIndexHeader">
          <Tag name={tag} notAsLink />
        </h1>
      </Header>

      <PostList posts={posts.map(n => n.node)} />

    </Layout>
  )
}
