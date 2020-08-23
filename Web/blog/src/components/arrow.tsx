import React from "react"
import styled from "styled-components"

type Props = {
  point: "left" | "right"
  colour?: string
}

const Container = styled.div`
  width: 100%;
  display: inline-flex;
`

const Line = styled.div`
  margin-top: 10px;
  height: 4px;
  flex-grow: 1;
`

const LeftArrow = styled.div`
  width: 0;
	height: 0;
  border-top: 12px solid transparent;
	border-bottom: 12px solid transparent;
	border-right: 26px solid blue;
`
const RightArrow = styled.div`
  width: 0;
	height: 0;
  border-top: 12px solid transparent;
	border-bottom: 12px solid transparent;
	border-left: 26px solid blue;
`

export default (p: Props) => {
  const colour = p.colour || "#000"
  return p.point == "left" ? (
    <Container>
      <LeftArrow style={{borderRightColor: colour}} />
      <Line style={{background: colour}} />
    </Container>
  ) : (
      <Container>
      <Line style={{background: colour}} />
      <RightArrow style={{borderLeftColor: colour}} />
    </Container>
  )
}
