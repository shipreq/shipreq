import { graphql } from "gatsby"

export const PostNode = graphql`
  fragment PostNode on Mdx {
    id
    frontmatter {
      date
      desc
      hn
      reddit
      slug
      tags
      title
      twitter
    }
  }
`
