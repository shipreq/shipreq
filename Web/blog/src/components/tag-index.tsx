import { useStaticQuery, graphql } from "gatsby"
import R from "../utils/responsive"
import React from "react"
import styled from "styled-components"
import Tag from "./tag"

const Container = styled.section`
`

const Item = styled.div`
  ${R.small`
    display: inline;
  `}
  & ~ & {
    ${R.phone`
      margin-left: 1.7ex;
    `}
    ${R.phoneWide`
      margin-top: 0.2em;
    `}
    ${R.tablet`
      margin-top: 0.7em;
    `}
    ${R.desktop`
      margin-top: 0.4em;
    `}
  }
`

export default () => {

  type Query = {
    allMdx: {
      tags: [{
        name: string
      }]
    }
  }

  const query: Query = useStaticQuery(graphql`
    query {
      allMdx(filter: {fileAbsolutePath: {glob: "**/posts/*"}}) {
        tags: group(field: frontmatter___tags) {
          name: fieldValue
        }
      }
    }
  `)

  const tags = query.allMdx.tags.map(t => t.name).sort()

  return (
    <Container>

      {tags.map(tag => (

        <Item key={tag}>
          <Tag name={tag} />
        </Item>

      ))}
    </Container>
  )
}
