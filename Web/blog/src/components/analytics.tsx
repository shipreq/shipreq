import { GoogleAnalytics, StatCounter } from "../config/types"
import { Helmet } from "react-helmet"
import { minifyJs } from "../utils/utils"
import { useStaticQuery, graphql } from "gatsby"
import React from "react"

export default function() {

  type Query = {
    site: {
      siteMetadata: {
        analytics: {
          googleAnalytics: GoogleAnalytics | null
          statCounter: StatCounter | null
        }
      }
    }
  }

  const { site: {siteMetadata: {analytics: a}}} = useStaticQuery<Query>(
    graphql`
      query {
        site {
          siteMetadata {
            analytics {
              googleAnalytics {
                trackingId
                jsUrl
                disabled
              }
              statCounter {
                project
                security
                https
                jsUrl
                disabled
              }
            }
          }
        }
      }
    `
  )

  function googleAnalytics(g: GoogleAnalytics) {
    const setup = minifyJs(`
      window.dataLayer=window.dataLayer||[];
      function gtag(){dataLayer.push(arguments)}
      gtag('js',new Date());
      gtag('config','${g.trackingId}');
    `)
    return (
      <Helmet>
        <script type="text/javascript" defer={false} src={g.jsUrl} async/>
        <script type="text/javascript" defer={false}>{setup}</script>
      </Helmet>
    )
  }

  function statCounter(s: StatCounter) {
    // Important notes:
    // 1. Use "var" in settings and not "const" so that it's idempotent
    // 2. Don't make the script tags async, else you get the "Failed to write to document" error
    // 3. When testing locally, make sure uBlock is disabled
    const settings = minifyJs(`
      var sc_https='${s.https ? 1 : 0}',
      sc_invisible=1,
      sc_remove_link=1,
      sc_security='${s.security}',
      sc_project=${s.project}
    `)
    return (
      <Helmet>
        <script type="text/javascript" defer={false}>{settings}</script>
        <script type="text/javascript" defer={false} src={s.jsUrl}/>
      </Helmet>
    )
  }

  return (<>
    {a.googleAnalytics && !a.googleAnalytics.disabled && googleAnalytics(a.googleAnalytics)}
    {a.statCounter     && !a.statCounter    .disabled && statCounter    (a.statCounter    )}
  </>)
}