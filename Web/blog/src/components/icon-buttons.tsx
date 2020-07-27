import { IconButton, Props as IconButtonProps } from "./icon-button"
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
  margin: 0;
`

export default (p: Props) => (
  <Container>
    {
      p.buttons.map(b => <IconButton key={b.icon} {...b} />)
    }
  </Container>
)
