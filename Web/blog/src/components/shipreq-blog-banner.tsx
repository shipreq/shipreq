import { Link as GatsbyLink } from "gatsby"
import A from "./a"
import React from "react"
import ShipreqBanner from "./shipreq-banner"
import styled from "styled-components"

const Link = styled(GatsbyLink)`
  display: inline-block;
  width: 100%;
`

const Title = styled.h1`
  width: 36%;
  margin: .1rem 0 0 auto !important;
  text-align: right;
  line-height: 0;
`

class Blog extends React.Component<{}, {hover: boolean}> {
  constructor(props: {}) {
    super(props)
    this.state = {hover: false}
    this.onHover = this.onHover.bind(this)
    this.onExit = this.onExit.bind(this)

  }

  onHover() {
    this.setState({hover: true})
  }

  onExit() {
    this.setState({hover: false})
  }

  render() {
    const colour = this.state.hover ? "#d90166" : "#00549c"
    return (
      <svg viewBox="0 0 85 33" onMouseEnter={this.onHover} onMouseLeave={this.onExit}>
        <text
          x="0"
          y="26"
          letterSpacing="3.6px"
          style={{
            fontFamily: "'Nunito Sans', sans-serif",
            fill: colour,
          }}
        >Blog</text>
      </svg>
    )
  }
}



export default function(): JSX.Element {
  return (
    <>
      <A href="https://shipreq.com">
        <ShipreqBanner width="100%" />
      </A>
      <Title>
        <Link to="/">
          <Blog />
        </Link>
      </Title>
    </>
  )
}
