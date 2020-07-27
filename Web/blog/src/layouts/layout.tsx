import { SEO, Props as SeoProps } from "../components/seo"
import Analytics from "../components/analytics"
import React from "react"

type Props = {
  seo     : SeoProps
  children: React.ReactNode
}

export default function(p: Props) {
  return (<>
    <Analytics />
    <SEO {...p.seo} />
    {p.children}
  </>)
}
