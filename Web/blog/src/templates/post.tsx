import { graphql } from "gatsby"
import { MDXProvider } from "@mdx-js/react"
import { MDXRenderer } from "gatsby-plugin-mdx"
import { Node, PageContext } from "../config/posts"
import { pathForPost } from "../utils/routes"
import { Props as SeoProps } from "../components/seo"
import Author from "../components/author"
import Layout from "../layouts/focused"
import MdxComponents from "../components/mdx_components"
import PostAttr from "../components/post-attributes"
import PostShare from "../components/post-share"
import PostSiblingNav from "../components/post-sibling-nav"
import React from "react"
import styled from "styled-components"

export const pageQuery = graphql`
  query PostPageQuery($id: String) {
    mdx(id: { eq: $id }) {
      ...PostNode
      body
    }
  }
`

type Props = {
  data: {
    mdx: Node & { body: string }
  }
  pageContext: PageContext
}

const Header = styled.header`
  margin-bottom: 2rem;
`

const Title = styled.h1`
  font-size: 220%;
  margin-bottom: 0;
`

const Footer = styled.footer`
  border-top: solid 1px #888;
  margin-top: 3rem;
  padding-top: 1rem;
`

export default function({ data, pageContext }: Props) {
  const post = data.mdx
  const { date, title, tags } = post.frontmatter

  const seo: SeoProps =  {
    article : true,
    desc    : post.frontmatter.desc,
    path    : pathForPost(post),
    subtitle: title,
  }

  return (
    <Layout seo={seo}>
      <article className="post">

        <Header>
          <Title>{title}</Title>
          <PostAttr date={date} tags={tags} />
          <PostShare post={post} pos="top" />
        </Header>

        <section className="body">
          <MDXProvider components={MdxComponents}>
            <MDXRenderer>{post.body}</MDXRenderer>
          </MDXProvider>
        </section>

        <PostShare post={post} pos="bottom" />

        <Footer>
          <Author />
          <PostSiblingNav pageContext={pageContext} />
        </Footer>

      </article>
    </Layout>
  )
};
