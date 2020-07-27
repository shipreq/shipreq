import IconButtons from "./icon-buttons"
import React from "react"
import site from "../config/site"

export default () => (
  <IconButtons buttons={
    [
      { icon: "twitter", href: site.twitter.url  },
      { icon: "reddit",  href: site.reddit.url   },
      { icon: "email",   href: site.email.mailto },
      { icon: "rss",     href: site.rssPath      },
    ]
  } />
)
