import React from "react"

/** Only supports a tiny subset of minification.
 * Meant for when you've written JS in a multiline string literal.
 */
export function minifyJs(js: string): string {
  return js
    .trim()
    .replace(/(?<=[;,}])\s+/gm, "")
    .replace(/;$/, "")
}

// Shitty hack to ensure exhaustivity
export function exhaustiveCheck(arg: never): void {}

export function encodeQuery(q: {[k: string]: string}): string {
  const comps: Array<string> = []
  for (let k in q) {
    const v = q[k]
    comps.push(`${k}=${encodeURIComponent(v)}`)
  }
  return "?" + comps.join("&")
}

export function urlWithQuery(url: string, query: {[k: string]: string}): string {
  return url + encodeQuery(query)
}

export function intersperse(elements: Array<React.ReactNode>, sep: React.ReactNode): React.ReactNode {
  const a = []
  let first = true
  for (let e in elements) {
    if (first)
      first = false
    else
      a.push(sep)
    a.push(e)
  }
  return (<React.Fragment children={a} />)
}
