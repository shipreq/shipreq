import { graphql } from "gatsby"
import { MDXProvider } from "@mdx-js/react"
import { MDXRenderer } from "gatsby-plugin-mdx"
import { Node, PageContext } from "../config/post"
import { pathForPost } from "../utils/routes"
import { Props as SeoProps } from "../components/seo"
import A from "../components/a"
import Author from "../components/author"
import Date from "../components/date"
import Layout from "../layouts/focused"
import PostShare from "../components/post-share"
import PostSiblingNav from "../components/post-sibling-nav"
import React from "react"
import styled from "styled-components"
import TagList from "../components/tag-list"

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

const components = {
  A,
  ScalaJS: () => <A href="https://www.scala-js.org">Scala.JS</A>,
  ShipReq: () => <A href="https://shipreq.com">ShipReq</A>,
}

const Article = styled.article`
  line-height: 1.5em;
  .huddle > li {
    margin-bottom: 0;
  }
`
const Title = styled.h1`
  display: block;
  font-weight: bold;
  border-bottom: solid 1px #ccc;
  color: #933;
  margin-bottom: 0;
  font-size: 250%;
`

const Header = styled.header`
  margin-bottom: 1rem;
`

const DateContainer = styled.header`
  color: #888;
  text-align: right;
  margin-top: 0.2em;
  font-size: 85%;
`

const footerGap = "1.8rem"

const Footer = styled.footer`
  border-top: solid 1px #ccc;
  margin-top: ${footerGap};
  padding-top: ${footerGap};
`

export default function({ data, pageContext }: Props) {
  const post  = data.mdx
  const title = post.frontmatter.title
  const tags  = post.frontmatter.tags

  const seo: SeoProps =  {
    article : true,
    desc    : post.frontmatter.desc,
    path    : pathForPost(post),
    subtitle: title,
  }

  return (
    <Layout seo={seo}>
      <Article>

        <Header>
          <Title>{title}</Title>
          <DateContainer>
            <Date date={post.frontmatter.date} />
          </DateContainer>
        </Header>

        <MDXProvider components={components}>
          <MDXRenderer>{post.body}</MDXRenderer>
        </MDXProvider>

        <Footer>

          <Author />

          <PostShare post={post} />
          <PostSiblingNav pageContext={pageContext} />

        </Footer>

      </Article>
    </Layout>
  )
};
