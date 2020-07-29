import { Props as SeoProps } from "../components/seo"
import ContactLinks from "../components/contact-links"
import Copyright from "../components/copyright"
import Layout from "./layout"
import PageIndex from "../components/page-index"
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
  tagIndex : 't',
  pageIndex: 'p',
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

  ${R.small`
    grid-template-rows: auto auto auto auto auto 1fr;
    grid-template-areas:
      "${GridArea.banner}"
      "${GridArea.contact}"
      "${GridArea.tagIndex}"
      "${GridArea.pageIndex}"
      "${GridArea.main}"
      "${GridArea.copyright}"
    ;
  `}
  ${R.notSmall`
    grid-template-rows: auto auto auto auto 1fr;
    grid-template-columns: auto 1fr;
    grid-template-areas:
      "${GridArea.banner}    ${GridArea.main}"
      "${GridArea.contact}   ${GridArea.main}"
      "${GridArea.tagIndex}  ${GridArea.main}"
      "${GridArea.pageIndex} ${GridArea.main}"
      "${GridArea.copyright} ${GridArea.main}"
    ;
  `}

  ${R.phone`
    padding: 0.8rem;
  `}
  ${R.phoneWide`
    row-gap: 0.8rem;
    column-gap: 1.6rem;
    padding: 0.8rem;
  `}
  ${R.tablet`
    column-gap: 2.4rem;
    padding: 1rem;
  `}
  ${R.desktop`
    column-gap: 4.8rem;
    max-width: 1200px;
    padding: 1rem;
  `}
`

const BannerCell = styled.header`
  grid-area: ${GridArea.banner};
`

const BannerWrapper = styled.div`
  ${R.phoneWide`
    width:200px;
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
    margin: 1rem 0 0 0;
  `}
  ${R.phoneWide`
  `}
  ${R.tablet`
    margin: 1rem 0;
  `}
  ${R.desktop`
    margin: 2rem 0;
  `}
`

const TagIndexCell = styled.section`
  grid-area: ${GridArea.tagIndex};
  text-align: right;
  ${R.phone`
    margin-top: 0.2rem;
  `}
`

const PageIndexCell = styled.section`
  grid-area: ${GridArea.pageIndex};
  text-align: right;
  ${R.phone`
    margin-top: 0.2rem;
  `}
  ${R.tablet`
    margin: 1rem 0;
  `}
  ${R.desktop`
    margin: 2rem 0;
  `}
`

const CopyrightCell = styled.footer`
  grid-area: ${GridArea.copyright};
  align-self: end;
  text-align: right;
  margin-top: 1rem;
  ${R.phoneWide`
    font-size:90%;
  `}
`

const MainCell = styled.main`
  grid-area: ${GridArea.main};
  ${R.phone`
    margin-top: 1.4rem;
  `}
  ${R.phoneWide`
    margin-top: 2rem;
  `}
  ${R.tablet`
    margin-top: 2.4rem;
  `}
  ${R.desktop`
    margin-top: 2.8rem;
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

        <TagIndexCell>
          <TagIndex />
        </TagIndexCell>

        <PageIndexCell>
          <PageIndex />
        </PageIndexCell>

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
