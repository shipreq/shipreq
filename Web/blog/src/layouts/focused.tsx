import { Props as SeoProps } from "../components/seo"
import ContactLinks from "../components/contact-links"
import Copyright from "../components/copyright"
import Layout from "./layout"
import R from "../utils/responsive"
import React from "react"
import ShipReqBanner from "../components/shipreq-blog-banner"
import styled from "styled-components"
import TagIndex from "../components/tag-index"

type Props = {
  seo     : SeoProps
  children: React.ReactNode
}

const GridArea = {
  banner   : 'b',
  contact  : 'c',
  copyright: 'l',
  tagList  : 't',
  main     : 'm',
}

const Container = styled.div`
  display: grid;
  align-content: stretch;
  align-items: start;
  justify-content: stretch;
  justify-items: stretch;
  margin: 0 auto;
  min-height: 100vh;

  ${R.phoneAny`
    grid-template-rows: auto auto auto auto 1fr;
    grid-template-areas:
      "${GridArea.banner}"
      "${GridArea.main}"
      "${GridArea.contact}"
      "${GridArea.tagList}"
      "${GridArea.copyright}"
    ;
  `}
  ${R.notPhone`
    grid-template-rows: auto auto 1fr;
    grid-template-areas:
      "${GridArea.banner}    ${GridArea.main}"
      "${GridArea.contact}   ${GridArea.main}"
      "${GridArea.tagList}   ${GridArea.main}"
      "${GridArea.copyright} ${GridArea.main}"
    ;
  `}

  ${R.phone`
    padding: 0.5rem;
  `}
  ${R.phoneWide`
    padding: 1rem;
  `}
  ${R.tablet`
    padding: 1rem;
    column-gap: 2.4rem;
  `}
  ${R.desktop`
    padding: 1rem;
    column-gap: 4.8rem;
    max-width: 1200px;
  `}
`

const BannerCell = styled.header`
  grid-area: ${GridArea.banner};
`

const BannerWrapper = styled.div`
  ${R.phoneWide`
    width:400px;
    margin-left: auto;
  `}
  ${R.tablet`
    width:230px;
  `}
  ${R.desktop`
    width:300px;
  `}
`

const ContactCell = styled.section`
  grid-area: ${GridArea.contact};
  align-self: end;
  justify-self: end;
  ${R.phone`
    display:none;
  `}
  ${R.phoneWide`
    display:none;
  `}
  ${R.tablet`
    margin: 1rem 0;
  `}
  ${R.desktop`
    margin: 2rem 0;
  `}
`

const TagListCell = styled.section`
  grid-area: ${GridArea.tagList};
  text-align: right;
  ${R.phone`
    display:none;
  `}
  ${R.phoneWide`
    display:none;
  `}
`

const CopyrightCell = styled.footer`
  grid-area: ${GridArea.copyright};
  align-self: end;
  text-align: right;
  margin-top: 1rem;
`

const MainCell = styled.main`
  grid-area: ${GridArea.main};
  ${R.phone`
    margin-top: 2rem;
  `}
  ${R.phoneWide`
  `}
  ${R.tablet`
  `}
  ${R.desktop`
  `}
`

export default function(p: Props) {
  return (
    <Layout seo={p.seo}>
      <Container>

        <BannerCell>
          <BannerWrapper>
            <ShipReqBanner />
          </BannerWrapper>
        </BannerCell>

        <ContactCell>
          <ContactLinks />
        </ContactCell>

        <TagListCell>
          <TagIndex />
        </TagListCell>

        <CopyrightCell>
          <Copyright />
        </CopyrightCell>

        <MainCell>
          {p.children}
        </MainCell>

      </Container>
    </Layout>
  )
}
