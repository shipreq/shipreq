import React, { ReactNode } from "react"
import styled from "styled-components"
import Tag from "./tag"
import { intersperse, renderArray } from "../utils/utils"

type Props = {
  tags: Array<string>
  notAsLink?: boolean
  separator?: ReactNode
  style?: object
}

const Item = styled.span`
  & ~ & {
    margin-left: 1ex;
  }
`

function render(p: Props): JSX.Element {

  if (p.separator) {
    const tags =
      p.tags.sort().map(tag => (
        <Tag name={tag} style={p.style} notAsLink={p.notAsLink} />
      ))
      return intersperse(tags, p.separator)

  } else {
    const tags =
      p.tags.sort().map(tag => (
        <Item key={tag}>
          <Tag name={tag} style={p.style} notAsLink={p.notAsLink} />
        </Item>
    ))
    return renderArray(tags)
  }
}

export default render