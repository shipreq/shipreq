import { graphql } from "gatsby"
import { Helmet } from "react-helmet"
import React from "react"
import SEO from "../components/seo"
import styled from "styled-components"
import tileQuestionSvg from "../images/tile-question.svg"

export const pageQuery = graphql`
  query {
    site {
      siteMetadata {
        description
        title
      }
    }
  }
`

type Query = {
  data: {
    site: {
      siteMetadata: {
        description: string
        title: string
      }
    }
  }
}

const Container = styled.div`
  height: 66.67vh;
  width: 100vw;
  display: flex;
`

export default function({ data }: Query) {
  const md = data.site.siteMetadata

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

          #a {font-size:37vw; font-weight:bold}
          #b {font-size:9.75vw}
          #c {font-size:5.2vw; margin-top:3.7em}

          @media screen and (min-width: 512px) {
            #a {font-size:9rem}
            #b {font-size:2.69rem}
            #c {font-size:1.2rem}
          }
        `}</style>
      </Helmet>

      <SEO
        article = {false}
        desc    = {md.description}
        path    = {null}
        title   = {`404 | ${md.title}`}
      />
      <main>
        <div id="a">404</div>
        <div id="b">Page not found.</div>
        <div id="c"><a href="/">Let's go home...</a></div>
      </main>
    </Container>
  )
}
