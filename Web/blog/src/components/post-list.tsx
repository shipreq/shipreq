import { Link } from "gatsby"
import { Node as Post } from "../config/post"
import { pathForPost } from "../utils/routes"
import Date from "./date"
import R from "../utils/responsive"
import React from "react"
import styled from "styled-components"
import TagList from "./tag-list"

type Props = {
  posts: Array<Post>
}

const Item = styled.div`
  margin-bottom: 2rem;
  ${R.phoneWide`
    margin-bottom: 1.2rem;
  `}
`

const Attributes = styled.div`
  opacity: 0.6;
  font-size: 80%;
  text-align: right;
`

const TagListWrapper = styled.span`
  margin: 0 1ex;
`

const Header = styled.h2`
  margin-bottom: 0.2em;
`

const Desc = styled.p`
  line-height: 1.45em;
  color: #555;
  margin: 0;
`

function renderPost(post: Post) {

  const tags: Array<string> =
    post.frontmatter.tags

  return (
    <Item key={post.id}>

      <Header>
        <Link to={pathForPost(post)}>{post.frontmatter.title}</Link>
      </Header>

      <Desc>
        {post.frontmatter.desc}
      </Desc>

      <Attributes>
        <TagListWrapper><TagList tags={tags} /></TagListWrapper>
        <Date date={post.frontmatter.date} />
      </Attributes>

    </Item>
  )
}

export default ({ posts } : Props) => {

  return (
    <div>
      {posts.map(renderPost)}
    </div>
  )
}
