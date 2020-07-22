import React from "react"
import kebabCase from "lodash/kebabCase"
import { Link } from "gatsby"

export const pathForPost: (arg: {slug: string} | {node: any}) => string =
  (arg) => {
    const { node, slug } = arg as { node: any, slug?: string }
    const s = slug || node.frontmatter.slug
    return `/post/${s.replace(/^\/+/, '')}`
  }

export const pathForTag: (name: string) => string =
  (name) => `/tag/${kebabCase(name)}`

export const pathForTagIndex =
  "/tag"

export const linkToTagIndex =
  <Link to={pathForTagIndex}>Tags</Link>
