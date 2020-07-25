import { Helmet } from "react-helmet"
import React from "react"
import { SEO } from "../components/seo"
import styled from "styled-components"
import tileQuestionSvg from "../images/tile-question.svg"

const Container = styled.div`
  height: 66.67vh;
  width: 100vw;
  display: flex;
`

export default function() {

  return (
    <Container>
      <Helmet>
        <style>{`
          body {
            background: url(${tileQuestionSvg}) 0/14rem, #fbfcfd;
          }

          main>div {line-height:1em}

          a {font-weight:bold}

          main {margin:auto; padding:0 2vw}

          #a {font-size:37vw; font-weight:bold; margin-top:3rem; margin-bottom:0.3rem;}
          #b {font-size:9.75vw; margin:0}
          #c {font-size:5.2vw; margin-top:4rem}

          @media screen and (min-width: 512px) {
            #a {font-size:9rem}
            #b {font-size:2.69rem}
            #c {font-size:1.2rem}
          }
        `}</style>
      </Helmet>

      <SEO subtitle="404" />

      <main>
        <div id="a">404</div>
        <div id="b">Page not found.</div>
        <div id="c"><a href="/">Let's go home...</a></div>
      </main>

    </Container>
  )
}
