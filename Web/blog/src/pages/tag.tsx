import { Helmet } from "react-helmet"
import { Link, graphql } from "gatsby"
import { pathForTag } from "../utils/routes"
import React from "react"
import sortBy from "lodash/sortBy"

export const pageQuery = graphql`
  query {
    site {
      siteMetadata {
        title
      }
    }
    allMdx {
      tags: group(field: frontmatter___tags) {
        name: fieldValue
        totalCount
      }
    }
  }
`

type Props = {
  data: {
    allMdx: {
      tags: [
        {
          name: string
          totalCount: number
        }
      ]
    }
    site: {
      siteMetadata: {
        title: string
      }
    }
  }
}

export default function({ data }: Props) {

  const tags = sortBy(data.allMdx.tags, 'name')

  return (
    <div>
      <Helmet title={data.site.siteMetadata.title} />
      <div>
        <h1>Tags</h1>
        <ul>
          {tags.map(tag => (
            <li key={tag.name}>
              <Link to={pathForTag(tag.name)}>
                {tag.name} ({tag.totalCount})
              </Link>
            </li>
          ))}
        </ul>
      </div>
    </div>
  )
}
