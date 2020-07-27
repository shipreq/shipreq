import React from "react"
import site from "../config/site"
import styled from "styled-components"

const Copyright = styled.div`
  color: #aaa;
  font-size: 75%;
`

export default function() {
  return (
    <Copyright>
      {site.copyright1}
      <br />
      {site.copyright2}
    </Copyright>
  )
}
