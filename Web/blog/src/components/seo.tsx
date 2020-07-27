import { Helmet } from "react-helmet"
import { useStaticQuery, graphql } from "gatsby"
import React from "react"
import site from "../config/site"

export type Props = {
  article?: boolean | null
  title?: string | null
  subtitle?: string | null
  desc?: string | null
  path?: string | null
}

export function SEO(p: Props) {

  type Query = {
    card: {
      publicURL: string
    }
  }

  const { card } = useStaticQuery<Query>(
    graphql`
      query {
        card: file(relativePath: { eq: "logo-title-1024.png" }) {
          publicURL
        }
      }
    `
  )

  const article = p.article || false

  let title = p.title || site.title
  if (p.subtitle)
    title = `${p.subtitle} | ${title}`

  const desc = p.desc || site.description

  const url = p.path && `${site.siteUrl}${p.path.replace(/^\/*/, "/")}`.replace(/\/+$/, "")

  return (<>
    <Helmet title={title} defer={false} />
    <Helmet>
      <html lang="en" />
      <meta name="description" content={desc} />

      <meta name="twitter:card"        content="summary_large_image" />
      <meta name="twitter:description" content={desc} />
      <meta name="twitter:image"       content={card.publicURL} />
      <meta name="twitter:site"        content={site.twitter.handle} />
      <meta name="twitter:title"       content={title} />
      {article && <meta property="twitter:creator" content={site.japgolly.twitter.handle} />}

      <meta property="og:description" content={desc} />
      <meta property="og:image"       content={card.publicURL} />
      <meta property="og:locale"      content={site.locale} />
      <meta property="og:site_name"   content={site.title} />
      <meta property="og:title"       content={title} />
      <meta property="og:type"        content={article ? "article" : "website"} />
      {url && <meta property="og:url" content={url} />}

    </Helmet>
  </>)
}