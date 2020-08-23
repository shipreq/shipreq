import Date from "./date"
import React from "react"
import styled from "styled-components"
import TagList from "./tag-list"

const Attributes = styled.div`
  font-size: 16px;
  margin-top: .4em;
  margin-bottom: .4em;
`

const DateStyle = styled.span`
  color: #888;
`

const AttributeSeparatorStyle = styled.span`
  color: #ddd;
  margin: 0 1.7ex;
`

const AttrSep = (<AttributeSeparatorStyle>|</AttributeSeparatorStyle>)

type Props = {
  date: string
  tags: Array<string>
}

export default (p: Props) => {
  return (
    <Attributes>
      <DateStyle><Date date={p.date} /></DateStyle>
      {AttrSep}
      <TagList tags={p.tags} separator={AttrSep} style={{opacity: 0.7}} />
    </Attributes>
  )
}
