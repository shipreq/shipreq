import React from "react"
import styled from "styled-components"
import Tag from "./tag"

type Props = {
  tags: Array<string>
  notAsLink?: boolean
}

const Item = styled.span`
  & ~ & {
    margin-left: 1ex;
  }
`

export default (p: Props) => {

  const tags = p.tags.sort()

  return (
    <>
      {tags.map(tag => (

        <Item key={tag}>
          <Tag name={tag} notAsLink={p.notAsLink} />
        </Item>

      ))}
    </>
  )
}
