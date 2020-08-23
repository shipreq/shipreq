import { Link } from "gatsby"
import { Node, PageContext } from "../config/posts"
import { pathForPost } from "../utils/routes"
import Arrow from "./arrow"
import R from "../utils/responsive"
import React from "react"
import styled from "styled-components"

type Props = {
  pageContext: PageContext
}

const Container = styled.nav`
  display: flex;
  width: 100%;
  margin-top: 2rem;
`

const gapPhone     = '2ex'
const gapPhoneWide = '5ex'
const gapTablet    = '6ex'
const gapDesktop   = '8ex'

const MiddleGap = styled.div`
  ${R.phone    `margin: 0 ${gapPhone}`}
  ${R.phoneWide`margin: 0 ${gapPhoneWide}`}
  ${R.tablet   `margin: 0 ${gapTablet}`}
  ${R.desktop  `margin: 0 ${gapDesktop}`}
  border-left: solid 2px #ccc;
`

const LeftNav = styled.div`
  ${R.phone    `width: calc(50% - ${gapPhone})`}
  ${R.phoneWide`width: calc(50% - ${gapPhoneWide})`}
  ${R.tablet   `width: calc(50% - ${gapTablet})`}
  ${R.desktop  `width: calc(50% - ${gapDesktop})`}
  align-self: flex-end;
`

const RightNav = styled.div`
  ${R.phone    `width: calc(50% - ${gapPhone})`}
  ${R.phoneWide`width: calc(50% - ${gapPhoneWide})`}
  ${R.tablet   `width: calc(50% - ${gapTablet})`}
  ${R.desktop  `width: calc(50% - ${gapDesktop})`}
  text-align: right;
`

const ArrowCommon = styled.div`
  max-width: 24ex;
`
const arrowGap = "1.5rem"
const ArrowLeft = styled(ArrowCommon)`
  margin-top: ${arrowGap};
`
const ArrowRight = styled(ArrowCommon)`
  margin-left: auto;
  margin-bottom: ${arrowGap};
`

const NodeLinkStyle = styled.div`
  & a:not(:hover) {
    color: #000;
  }
  font-size: 2rem;
  line-height: 2.8rem;
  font-weight: 600;
`

const NodeLink = ({node} : {node: Node}) =>
  <NodeLinkStyle>
    <Link to={pathForPost(node)}>{node.frontmatter.title}</Link>
  </NodeLinkStyle>

export default ({ pageContext: ctx }: Props) => (ctx.older || ctx.newer) && (
  <Container>

    <LeftNav>
      {
        ctx.older && (<>
          <NodeLink node={ctx.older} />
          <ArrowLeft><Arrow point="left" /></ArrowLeft>
        </>)
      }
    </LeftNav>

    <MiddleGap />

    <RightNav>
      {
        ctx.newer && (<>
          <ArrowRight><Arrow point="right" /></ArrowRight>
          <NodeLink node={ctx.newer} />
        </>)
      }
    </RightNav>

  </Container>
)
