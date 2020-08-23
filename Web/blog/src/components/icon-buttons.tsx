import { IconButton, Props as IconButtonProps } from "./icon-button"
import R from "../utils/responsive"
import React from "react"
import styled from "styled-components"

type Props = {
  buttons: Array<IconButtonProps>
}

const Container = styled.div`
  display: inline-flex;
  flex-flow: row wrap;
  flex-grow: 0;
  flex-shrink: 0;
  list-style: none;
  padding: 0;

  & > div:not(:first-child) {
    ${R.notDesktop`
      margin-left: 2ex;
    `}
    ${R.desktop`
      margin-left: 1.2ex;
    `}
  }
`

export default (p: Props) => (
  <Container>
    {
      p.buttons.map(b => <IconButton key={b.icon} {...b} />)
    }
  </Container>
)
