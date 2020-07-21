import React from "react"
import { Helmet } from "react-helmet"
import { useStaticQuery, graphql } from "gatsby"

type Props = {
  article: boolean,
  title: string,
  desc: string,
  path: string,
}

type Query = {
  site: {
    siteMetadata: {
      cardImageUrl: string
      locale: string
      rootUrl: string
      title: string
      twitterHandle: string
      author: {
        twitterHandle: string
      }
    }
  }
}

export default function(p: Props) {

  const { site: {siteMetadata: md} } = useStaticQuery<Query>(
    graphql`
      query {
        site {
          siteMetadata {
            cardImageUrl
            locale
            rootUrl
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

  const url = `${md.rootUrl}${p.path.replace(/^\/*/, "/")}`.replace(/\/+$/, "")

  return (<>
    <Helmet title={p.title} defer={false} />
    <Helmet>
      <html lang="en" />
      <meta name="description" content={p.desc} />

      <meta name="twitter:card"        content="summary_large_image" />
      <meta name="twitter:description" content={p.desc} />
      <meta name="twitter:image"       content={md.cardImageUrl} />
      <meta name="twitter:site"        content={md.twitterHandle} />
      <meta name="twitter:title"       content={p.title} />
      {p.article && <meta property="twitter:creator" content={md.author.twitterHandle} />}

      <meta property="og:description"      content={p.desc} />
      <meta property="og:image"            content={md.cardImageUrl} />
      <meta property="og:locale"           content={md.locale} />
      <meta property="og:site_name"        content={md.title} />
      <meta property="og:title"            content={p.title} />
      <meta property="og:type"             content={p.article ? "article" : "website"} />
      <meta property="og:url"              content={url} />

    </Helmet>
  </>)
}