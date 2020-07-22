import { graphql } from "gatsby"
import { Link } from "gatsby"
import { MDXProvider } from "@mdx-js/react"
import { MDXRenderer } from "gatsby-plugin-mdx"
import { pathForTag } from "../utils/routes"
import React from "react"
import SEO from "../components/seo"

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
        desc: string
      }
    }
  }
}

export default function PageTemplate({ data: { mdx } }: Query) {
  const title = mdx.frontmatter.title
  const tags = mdx.frontmatter.tags.sort()

  return (
    <div>
      <SEO
        article = {true}
        desc    = {mdx.frontmatter.desc}
        path    = {mdx.fields.path}
        title   = {title}
      />

      <h1>{title}</h1>

      <MDXProvider>
        <MDXRenderer>{mdx.body}</MDXRenderer>
      </MDXProvider>

      <ul>
        {tags.map(tag => (
          <li key={tag}>
            <Link to={pathForTag(tag)}>
              {tag}
            </Link>
          </li>
        ))}
      </ul>

    </div>
  )
};
