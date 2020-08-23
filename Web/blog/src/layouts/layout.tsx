import { SEO, Props as SeoProps } from "../components/seo"
import Analytics from "../components/analytics"
import Fonts from "../components/fonts"
import React from "react"

type Props = {
  seo     : SeoProps
  children: React.ReactNode
}

export default function(p: Props) {
  return (<>
    <Fonts />
    <Analytics />
    <SEO {...p.seo} />
    {p.children}
  </>)
}
