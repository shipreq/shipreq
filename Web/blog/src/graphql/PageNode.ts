import { graphql } from "gatsby"

export const PageNode = graphql`
  fragment PageNode on Mdx {
    id
    frontmatter {
      slug
      title
    }
  }
`
