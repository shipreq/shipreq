import { useStaticQuery, graphql } from "gatsby"
import { FixedObject } from "gatsby-image"
import A from "./a"
import Img from "gatsby-image"
import R from "../utils/responsive"
import React from "react"
import site from "../config/site"
import styled from "styled-components"

const Container = styled.section`
  display: flex;
`

const Avatar = styled(Img)`
  border-radius: 50%;
`

const TextContainer = styled.div`
  flex-grow: 1;
  ${R.small`
    margin-left: 0.5em;
  `}
  ${R.notSmall`
    margin-left: 1em;
  `}
`

const Title = styled.div`
  font-size: 90%;
  color: #444;
  font-weight: bold;
`

const Bio = styled.div`
  color: #777;
  font-size: 85%;
  line-height: 1.3em;
  hyphens: auto;
  overflow-wrap: break-word;
  text-align: justify;
  word-break: break-word;
  ${R.notPhone`
    margin-top: 0.4em;
  `}
`

export default function() {
  const author = site.japgolly

  type Query = {
    file: {
      childImageSharp: {
        fixed: FixedObject
      }
    }
  }

  const query = useStaticQuery<Query>(
    graphql`
      query {
        file(relativePath: { eq: "japgolly.jpg" }) {
          childImageSharp {
            fixed(width: 100, height: 100) {
              ...GatsbyImageSharpFixed
            }
          }
        }
      }
    `
  )

  return (
    <Container>

      <div>
        <Avatar
          fixed={query.file.childImageSharp.fixed}
          loading="eager"
          alt={`Avatar of ${author.name}`}
        />
      </div>

      <TextContainer>
        <Title>
          {`Written by `}
          <A href={author.link}>{author.name}</A>
        </Title>
        <Bio>{author.bio}</Bio>
      </TextContainer>

    </Container>
  )
}
