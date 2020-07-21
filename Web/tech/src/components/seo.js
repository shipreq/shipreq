import React from "react"
import { Helmet } from "react-helmet"
import { useStaticQuery, graphql } from "gatsby"

export default function({article, title, desc, path}) {

  const { site: {siteMetadata: md} } = useStaticQuery(
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

  const url = (md.rootUrl + path.replace(/^\/*/, "/")).replace(/\/+$/, "")

  return (
    <Helmet>
      <html lang="en" />
      <title>{title}</title>
      <meta name="description" content={desc} />

      <meta name="twitter:card"        content="summary_large_image" />
      <meta name="twitter:description" content={desc} />
      <meta name="twitter:image"       content={md.cardImageUrl} />
      <meta name="twitter:site"        content={md.twitterHandle} />
      <meta name="twitter:title"       content={title} />
      {article && <meta property="twitter:creator" content={md.author.twitterHandle} />}

      <meta property="og:description"      content={desc} />
      <meta property="og:image"            content={md.cardImageUrl} />
      <meta property="og:locale"           content={md.locale} />
      <meta property="og:site_name"        content={md.title} />
      <meta property="og:title"            content={title} />
      <meta property="og:type"             content={article ? "article" : "website"} />
      <meta property="og:url"              content={url} />

    </Helmet>
  )
}