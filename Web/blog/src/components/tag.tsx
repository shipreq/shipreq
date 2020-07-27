import { Link } from "gatsby"
import { pathForTag } from "../utils/routes"
import React from "react"
import styled from "styled-components"

type Props = {
  name: string
  notAsLink?: boolean
}

const Wrapper = styled.span`
  white-space: nowrap;
`

export default (p: Props) => {
  const title = <Wrapper>{"#" + p.name}</Wrapper>
  return (p.notAsLink
    ? title
    : <Link to={pathForTag(p.name)}>{title}</Link>
  )
}
