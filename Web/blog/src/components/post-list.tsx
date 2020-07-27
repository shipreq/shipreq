import { Link } from "gatsby"
import { Node as Post } from "../config/post"
import { pathForPost } from "../utils/routes"
import React from "react"
import styled from "styled-components"

type Props = {
  posts: Array<Post & { excerpt: string }>
}

const Item = styled.div`
  margin-bottom: 2rem;
`

const Header = styled.h2`
  margin-bottom: 0.2em;
`

const Excerpt = styled.p`
  line-height: 1.45em;
  color: #555;
`

export default ({ posts } : Props) => {
  return (
    <div>
      {posts.map(post => (
        <Item key={post.id}>

        <Header>
          <Link to={pathForPost(post)}>{post.frontmatter.title}</Link>
        </Header>

        <Excerpt>
          {post.excerpt}
        </Excerpt>

        </Item>
      ))}
    </div>
  )
}
