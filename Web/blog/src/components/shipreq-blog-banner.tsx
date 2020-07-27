import { Link as GatsbyLink } from "gatsby"
import A from "./a"
import R from "../utils/responsive"
import React from "react"
import ShipreqBanner from "./shipreq-banner"
import styled from "styled-components"

const Link = styled(GatsbyLink)`
  display: inline-block;
  width: 100%;
`

const Title = styled.h1`
  margin: 0;
  text-align: right;
  font-weight: bold;
  color: #00488c;
  ${R.phone`
    font-size: 250%;
  `}
  ${R.phoneWide`
    font-size: 160%;
  `}
  ${R.tablet`
    font-size: 180%;
  `}
  ${R.desktop`
    font-size: 230%;
  `}
`

export default function(): JSX.Element {
  return (
    <>
      <A href="https://shipreq.com">
        <ShipreqBanner width="100%" />
      </A>
      <Link to="/">
        <Title>Blog</Title>
      </Link>
    </>
  )
}
