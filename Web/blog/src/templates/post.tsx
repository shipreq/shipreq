import { graphql, Link } from "gatsby"
import { MDXProvider } from "@mdx-js/react"
import { MDXRenderer } from "gatsby-plugin-mdx"
import { Node, PageContext } from "../config/post"
import { pathForPost } from "../utils/routes"
import { Props as SeoProps } from "../components/seo"
import A from "../components/a"
import Author from "../components/author"
import Layout from "../layouts/focused"
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

const MrB = ({type}: {type: "app" | "github"}) => (
  (type == "app")
  ? <A href="https://japgolly.github.io/mr.boilerplate">Mr. Boilerplate</A>
  : <A href="https://github.com/japgolly/mr.boilerplate">Mr. Boilerplate</A>
)

const components = {
  A,
  MrB,
  About    : () => <Link to="/about">About</Link>,
  BooPickle: () => <A href="https://github.com/suzaku-io/boopickle">BooPickle</A>,
  Graal    : () => <A href="https://www.graalvm.org">GraalVM</A>,
  ScalaJS  : () => <A href="https://www.scala-js.org">Scala.JS</A>,
  SG       : () => <A href="https://github.com/japgolly/scala-graal">scala-graal</A>,
  ShipReq  : () => <A href="https://shipreq.com">ShipReq</A>,
  SJR      : () => <A href="https://github.com/japgolly/scalajs-react">scalajs-react</A>,
}

const Header = styled.header`
  margin-bottom: 1.5rem;
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
          <MDXProvider components={components}>
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
