import React from "react"

type Props = {
  href     : string
  children?: React.ReactNode
}

export default function(p: Props) {
  return <a href={p.href} target="_blank" rel="noopener">{p.children || p.href}</a>
}
