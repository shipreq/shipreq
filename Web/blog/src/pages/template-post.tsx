import { graphql } from "gatsby"
import { linkToTag } from "../utils/routes"
import { MDXProvider } from "@mdx-js/react"
import { MDXRenderer } from "gatsby-plugin-mdx"
import { Props as SeoProps } from "../components/seo"
import Layout from "../layouts/focused"
import A from "../components/a"
import Author from "../components/author"
import Date from "../components/date"
import React from "react"
import styled from "styled-components"

export const pageQuery = graphql`
  query BlogPostQuery($id: String) {
    mdx(id: { eq: $id }) {
      id
      body
      fields {
        path
      }
      frontmatter {
        tags
        title
        date
        desc
      }
    }
  }
`

type Query = {
  data: {
    mdx: {
      id: string
      body: string
      fields: {
        path: string
      }
      frontmatter: {
        tags: Array<string>
        title: string
        date: string
        desc: string
      }
    }
  }
}

const components = {
  A,
  ScalaJS: () => <A href="https://www.scala-js.org">Scala.JS</A>,
  ShipReq: () => <A href="https://shipreq.com">ShipReq</A>,
}

const Article = styled.article`
  line-height:1.5em;
  .huddle > li {
    margin-bottom:0;
  }
`
const Title = styled.h1`
  display:block;
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
  color:#888;
  text-align: right;
  margin-top: 0.2em;
  font-size: 85%;
`

const Footer = styled.footer`
`

export default function({ data: { mdx } }: Query) {
  // setLocale()

  const title = mdx.frontmatter.title
  const tags = mdx.frontmatter.tags.sort()
  // const date = moment.utc(mdx.frontmatter.date)

  const seo: SeoProps =  {
    article : true,
    desc    : mdx.frontmatter.desc,
    path    : mdx.fields.path,
    subtitle: title,
  }

  return (
    <Layout seo={seo}>
      <Article>

        <Header>
          <Title>{title}</Title>
          <DateContainer><Date date={mdx.frontmatter.date} /></DateContainer>
        </Header>

        <MDXProvider components={components}>
          <MDXRenderer>{mdx.body}</MDXRenderer>
        </MDXProvider>

        <Footer>
        </Footer>

        <Author />

        <ul>
          {tags.map(tag => (
            <li key={tag}>{linkToTag(tag)}</li>
          ))}
        </ul>

      </Article>
    </Layout>
  )
};
