import { Icon, IconName } from "./icon"
import React from "react"
import styled from "styled-components"

export type Props = {
  icon: IconName
  href: string
  inverse?: boolean
}

const buttonHeight = "2.1875rem"

const Container = styled.div`
  padding: 0;
  margin: 0;
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

const LinkBase = styled.a`
  border: 0;
  display: flex;
`

const backgroundNormal = "#fff"
const LinkNormal = styled(LinkBase)`
  color: #222;
  &:hover, &:focus {
    color: #d90166;
  }
`

const backgroundInverse = "#222"
const LinkInverse = styled(LinkBase)`
  color: #fff;
  &:hover, &:focus {
    color: #d90166;
  }
`

export const IconButton = (p: Props) => {
  const Link = p.inverse ? LinkInverse : LinkNormal
  const backgroundColor = p.inverse ? backgroundInverse : backgroundNormal
  return (
    <Container style={{backgroundColor}}>
      <Link href={p.href} target="_blank" rel="noopener">
        <Icon icon={p.icon} />
      </Link>
    </Container>
  )
}