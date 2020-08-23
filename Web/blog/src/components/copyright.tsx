import R from "../utils/responsive"
import React from "react"
import site from "../config/site"
import styled from "styled-components"

type Props = {
  layout?: "regular" | "focused"
}

const Copyright = styled.div`
  color: #aaa;
  font-size: 75%;
  line-height: 1.5em;
  letter-spacing: 0;
`

const FlattenUnlessDesktop = styled(Copyright)`
  ${R.phone`
    font-size: 70%;
  `}
  ${R.notDesktop`
    display: flex;
    justify-content: center;
    div:first-child {
      margin-right: .6ex;
    }
  `}
`

export default function(p: Props = {}) {
  return p.layout == "focused" ?
  (
    <FlattenUnlessDesktop>
      <div>{site.copyright1}</div>
      <div>{site.copyright2}</div>
    </FlattenUnlessDesktop>
  ) : (
    <Copyright>
      <div>{site.copyright1}</div>
      <div>{site.copyright2}</div>
    </Copyright>
  )
}
