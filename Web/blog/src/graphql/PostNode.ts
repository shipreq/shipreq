import { graphql } from "gatsby"

export const PostNode = graphql`
  fragment PostNode on Mdx {
    id
    frontmatter {
      date
      desc
      reddit
      slug
      tags
      title
      twitter
    }
  }
`
