import { graphql } from "gatsby"
import { MDXProvider } from "@mdx-js/react"
import { MDXRenderer } from "gatsby-plugin-mdx"
import { Node, PageContext } from "../config/posts"
import { pathForPage } from "../utils/routes"
import { Props as SeoProps } from "../components/seo"
import Layout from "../layouts/focused"
import MdxComponents from "../components/mdx_components"
import React from "react"

export const pageQuery = graphql`
  query PagePageQuery($id: String) {
    mdx(id: { eq: $id }) {
      ...PageNode
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

export default function({ data }: Props) {
  const path = data.mdx
  const { title } = path.frontmatter

  const seo: SeoProps =  {
    path    : pathForPage(path),
    subtitle: title,
  }

  return (
    <Layout seo={seo}>

      <main className="page">
        <MDXProvider components={MdxComponents}>
          <MDXRenderer>{path.body}</MDXRenderer>
        </MDXProvider>
      </main>

    </Layout>
  )
};
