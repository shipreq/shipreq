import { Node } from "../config/post"
import { Props as IconButtonProps } from "./icon-button"
import { urlForPost } from "../utils/routes"
import { urlWithQuery } from "../utils/utils"
import IconButtons from "./icon-buttons"
import R from "../utils/responsive"
import React from "react"
import site from "../config/site"
import styled from "styled-components"

type Props = {
  post: Node
}

const Container = styled.section`
  margin-top: 1em;
  ${R.phoneWide`
    margin-top: 0.5em;
  `}
`

const Header = styled.span`
  color: #666;
  margin-right: 1ex;
`

export default ({ post }: Props) => {
  const p   = post.frontmatter
  const url = urlForPost(post)

  const facebookUrl =
    urlWithQuery("https://www.facebook.com/sharer.php", {
      u: url,
    })

  const linkedInUrl =
    urlWithQuery("https://www.linkedin.com/shareArticle", {
      url,
      mini: "true",
      title: p.title.substr(0, 200),
      summary: p.desc.substr(0, 256),
      source: site.title,
    })

  const buttons: Array<IconButtonProps> = []
  if (p.twitter) buttons.push({ icon: "twitter", href: p.twitter })
  if (p.reddit)  buttons.push({ icon: "reddit", href: p.reddit })
  if (p.hn)      buttons.push({ icon: "hackerNews", href: p.hn })
  buttons.push({ icon: "facebook", href: facebookUrl })
  buttons.push({ icon: "linkedIn", href: linkedInUrl })

  return (
    <Container>
      <Header>Share / Discuss:</Header>
      <IconButtons buttons={buttons} />
    </Container>
  )
}
