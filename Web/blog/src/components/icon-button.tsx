import { Icon, IconName } from "./icon"
import React from "react"
import styled from "styled-components"

export type Props = {
  icon: IconName
  href: string
}

const buttonHeight = "2.1875rem"

const Container = styled.div`
  padding: 0;
  margin: 4px;
  display: flex;
  align-content: center;
  align-items: center;
  justify-content: center;
  width: ${buttonHeight};
  height: ${buttonHeight};
  line-height: ${buttonHeight};
  border-radius: 50%;
  text-align: center;
  border: 1px solid #ebebeb;
`

const Link = styled.a`
  border: 0;
  display: flex;
  color: #222;
  &:hover, &:focus {
    color: #5d93ff;
  }
`

export const IconButton = (p: Props) => (
  <Container>
    <Link href={p.href} target="_blank" rel="noopener">
      <Icon icon={p.icon} />
    </Link>
  </Container>
)
