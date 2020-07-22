import { Helmet } from "react-helmet"
import { useStaticQuery, graphql } from "gatsby"
import React from "react"

type Props = {
  article: boolean
  title: string
  desc: string
  path: string | null
}

export default function(p: Props) {

  type Query = {
    card: {
      publicURL: string
    }
    site: {
      siteMetadata: {
        locale: string
        siteUrl: string
        title: string
        twitterHandle: string
        author: {
          twitterHandle: string
        }
      }
    }
  }

  const { card, site: {siteMetadata: md} } = useStaticQuery<Query>(
    graphql`
      query {
        card: file(relativePath: {eq: "logo-title-1024.png"}) {
          publicURL
        }
        site {
          siteMetadata {
            locale
            siteUrl
            title
            twitterHandle
            author {
              twitterHandle
            }
          }
        }
      }
    `
  )

  const url = p.path && `${md.siteUrl}${p.path.replace(/^\/*/, "/")}`.replace(/\/+$/, "")

  return (<>
    <Helmet title={p.title} defer={false} />
    <Helmet>
      <html lang="en" />
      <meta name="description" content={p.desc} />

      <meta name="twitter:card"        content="summary_large_image" />
      <meta name="twitter:description" content={p.desc} />
      <meta name="twitter:image"       content={card.publicURL} />
      <meta name="twitter:site"        content={md.twitterHandle} />
      <meta name="twitter:title"       content={p.title} />
      {p.article && <meta property="twitter:creator" content={md.author.twitterHandle} />}

      <meta property="og:description" content={p.desc} />
      <meta property="og:image"       content={card.publicURL} />
      <meta property="og:locale"      content={md.locale} />
      <meta property="og:site_name"   content={md.title} />
      <meta property="og:title"       content={p.title} />
      <meta property="og:type"        content={p.article ? "article" : "website"} />
      {url && <meta property="og:url" content={url} />}

    </Helmet>
  </>)
}