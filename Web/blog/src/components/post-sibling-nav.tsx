import { Link } from "gatsby"
import { Node, PageContext } from "../config/post"
import { pathForPost } from "../utils/routes"
import React from "react"
import styled from "styled-components"

type Props = {
  pageContext: PageContext
}

const Container = styled.nav`
  display: flex;
  width: 100%;
  margin-top: 1rem;
`
const MiddleGap = styled.div`
  flex-grow: 1;
`

const PotentialLink = ({node, prefix, suffix} : {node: Node | null, prefix?: string, suffix?: string}) =>
  node ?
  <Link to={pathForPost(node)}>{`${prefix || ""}${node.frontmatter.title}${suffix || ""}`}</Link> :
  null

export default ({ pageContext: ctx }: Props) => (
  <Container>
    <PotentialLink node={ctx.older} prefix="← " />
    <MiddleGap />
    <PotentialLink node={ctx.newer} suffix=" →" />
  </Container>
)
