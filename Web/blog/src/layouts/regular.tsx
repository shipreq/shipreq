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
  min-width: 0;
  box-sizing: border-box;

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
    min-height: calc(100vh - 1.6rem);
  `}
  ${R.phoneWide`
    row-gap: 0.8rem;
    column-gap: 1.8rem;
    padding: 0.8rem;
    min-height: calc(100vh - 1.6rem);
  `}
  ${R.tablet`
    column-gap: 2.4rem;
    padding: 1rem;
    min-height: calc(100vh - 2rem);
  `}
  ${R.desktop`
    column-gap: 4.8rem;
    max-width: 1200px;
    padding: 1rem;
    min-height: calc(100vh - 2rem);
  `}
`

const BannerCell = styled.header`
  grid-area: ${GridArea.banner};
`

const BannerWrapper = styled.div`
  ${R.phoneWide`
    width:190px;
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
    margin-top: 0.8rem;
  `}
  ${R.phoneWide`
    margin-top: 0.6rem;
  `}
  ${R.tablet`
    margin-top: 3rem;
  `}
  ${R.desktop`
    margin-top: 3rem;
  `}
`

const TagIndexCell = styled.section`
  grid-area: ${GridArea.tagIndex};
  text-align: right;
  letter-spacing: 1px;
  ${R.phone`
    margin-top: 0.3rem;
  `}
  ${R.phoneWide`
    margin-top: 0.6rem;
  `}
  ${R.tablet`
    margin-top: 3rem;
  `}
  ${R.desktop`
    margin-top: 3rem;
  `}
`

const PageIndexCell = styled.section`
  grid-area: ${GridArea.pageIndex};
  text-align: right;
  letter-spacing: 1px;
  ${R.phone`
    margin-top: 0.3rem;
  `}
  ${R.phoneWide`
    margin-top: 0.6rem;
  `}
  ${R.tablet`
    margin-top: 3rem;
  `}
  ${R.desktop`
    margin-top: 3rem;
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
