import React from "react"
import { graphql } from "gatsby"
import { MDXProvider } from "@mdx-js/react"
import { MDXRenderer } from "gatsby-plugin-mdx"
import { Link } from "gatsby"
import SEO from "../components/seo"

const componentsUsed = { Link }

export default function PageTemplate({ data: { mdx } }) {

  const title = mdx.frontmatter.title

  return (
    <div>
      <SEO
        article = "1"
        desc    = {mdx.frontmatter.desc}
        title   = {title}
        path    = {mdx.fields.path}
      />
      <h1>{title}</h1>
      <MDXProvider components={componentsUsed}>
        <MDXRenderer>{mdx.body}</MDXRenderer>
      </MDXProvider>
    </div>
  )
};

export const pageQuery = graphql`
  query BlogPostQuery($id: String) {
    mdx(id: { eq: $id }) {
      id
      body
      fields {
        path
      }
      frontmatter {
        title
        desc
      }
    }
  }
`