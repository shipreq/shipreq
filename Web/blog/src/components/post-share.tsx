import { Node } from "../config/post"
import { urlForPost } from "../utils/routes"
import { Props as IconButtonProps } from "./icon-button"
import { urlWithQuery } from "../utils/utils"
import IconButtons from "./icon-buttons"
import React from "react"
import site from "../config/site"

type Props = {
  post: Node
}

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
  buttons.push({ icon: "facebook", href: facebookUrl })
  buttons.push({ icon: "linkedIn", href: linkedInUrl })

  return (
    <>
      <div>Share / Discuss: </div>
      <IconButtons buttons={buttons} />
    </>
  )
}
