import { Icon, IconName } from "./icon"
import React from "react"
import site from "../config/site"
import styled from "styled-components"

const Contacts = styled.ul`
  display: flex;
  flex-flow: row wrap;
  flex-grow: 0;
  flex-shrink: 0;
  list-style: none;
  padding: 0;
  margin: 0;
`

const buttonHeight = "2.1875rem"

const ContactItem = styled.li`
  padding: 0;
  margin: 4px;
  display: flex;
  align-content: center;
  align-items: center;
  justify-content: center;
  width: ${buttonHeight};
  height: ${buttonHeight};
  line-height: ${buttonHeight};
  border-radius: 50%;
  text-align: center;
  border: 1px solid #ebebeb;
`

const ContactLink = styled.a`
  border: 0;
  display: flex;
  color: #222;
  &:hover, &:focus {
    color: #5d93ff;
  }
`

type Contact = { icon: IconName; href: string }

const Contact = (c: Contact) => (
  <ContactItem>
    <ContactLink href={c.href} target="_blank" rel="noopener">
      <Icon icon={c.icon} />
    </ContactLink>
  </ContactItem>
)

export default function() {

  return (
    <Contacts>
      <Contact icon="twitter" href={site.twitter.url} />
      <Contact icon="reddit" href={site.reddit.url} />
      <Contact icon="email" href={site.email.mailto} />
      <Contact icon="rss" href={site.rssPath} />
    </Contacts>
  )
}
