import { IconName } from "./icon"
import { Node } from "../config/post"
import { Props as IconButtonProps } from "./icon-button"
import { urlForPost } from "../utils/routes"
import { urlWithQuery } from "../utils/utils"
import IconButtons from "./icon-buttons"
import React from "react"
import site from "../config/site"
import styled from "styled-components"

type Props = {
  pos: "top" | "bottom"
  post: Node
}

const TopContainer = styled.section`
`

const BottomContainer = styled.section`
  margin-top: 2em;
  text-align: center;
`

export default ({ pos, post }: Props) => {
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

  function addButton(icon: IconName, href: string): void {
    buttons.push({ icon, href, inverse: true })
  }

  if (p.twitter) addButton("twitter", p.twitter)
  if (p.reddit)  addButton("reddit", p.reddit)
  if (p.hn)      addButton("hackerNews", p.hn)
  addButton("facebook", facebookUrl)
  addButton("linkedIn", linkedInUrl)

  const Container = pos == "top" ? TopContainer : BottomContainer

  return (
    <Container>
      <IconButtons buttons={buttons} />
    </Container>
  )
}
