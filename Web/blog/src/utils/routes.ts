import { Node as Page } from "../config/pages"
import { Node as Post } from "../config/posts"
import kebabCase from "lodash/kebabCase"
import site from "../config/site"

const rootUrl = site.siteUrl.replace(/\/+$/, '')

export const pathForPage: (node: Page) => string =
  (node) => {
    const s = node.frontmatter.slug
    return `/${s.replace(/^\/+/, '')}`
  }

export const pathForPost: (node: Post) => string =
  (node) => {
    const s = node.frontmatter.slug
    return `/post/${s.replace(/^\/+/, '')}`
  }

export const urlForPost: (node: Post) => string =
  (node) => rootUrl + pathForPost(node)

export const pathForTag: (name: string) => string =
  (name) => `/tag/${kebabCase(name)}`
