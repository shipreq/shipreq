import { Link } from "gatsby"
import { pathForPostSlug } from "../utils/routes"
import A from "./a"
import React from "react"
import styled from "styled-components"

const MrB = ({type}: {type: "app" | "github"}) => (
  (type == "app")
  ? <A href="https://japgolly.github.io/mr.boilerplate" >Mr. Boilerplate</A>
  : <A href="https://github.com/japgolly/mr.boilerplate">Mr. Boilerplate</A>
)

const BlogPost = ({slug, title}: {slug: string, title: string}) =>
  <Link to={pathForPostSlug(slug)}>{title}</Link>

const sideBySideGap = "2ex"
const SideBySide = styled.div`
  display: flex;
  width: 100%;
  & > div {
    padding-bottom: 1em;
    width: 50%;
    flex-grow: 0;
  }
  & > div:first-child {
    padding-right: ${sideBySideGap};
  }
  & > div:not(:first-child) {
    border-left: solid 2px #e2e2e2;
    padding-left: ${sideBySideGap};
  }
`

const addClass: (cls: string) => React.FC<{}> =
  cls => props => (<div className={cls}>{props.children}</div>)

export default {
  A,
  BlogPost,
  MrB,
  SideBySide,

  About     : () => <Link to="/about">About</Link>,
  BooPickle : () => <A href="https://github.com/suzaku-io/boopickle">BooPickle</A>,
  Gatsby    : () => <A href="https://www.gatsbyjs.org">Gatsby</A>,
  Go        : () => <A href="https://golang.org">Go</A>,
  Graal     : () => <A href="https://www.graalvm.org">GraalVM</A>,
  Grafana   : () => <A href="https://grafana.com/oss/grafana">Grafana</A>,
  Haskell   : () => <A href="https://www.haskell.org">Haskell</A>,
  Heart     : () => <span style={{color:'#f10a0e'}}>❤️</span>,
  Katsokaa  : addClass("katsokaa"),
  NextJS    : () => <A href="https://nextjs.org">Next.js</A>,
  Nyaya     : () => <A href="https://github.com/japgolly/nyaya">Nyaya</A>,
  odersky   : () => <A href="https://twitter.com/odersky">Martin Odersky</A>,
  Prometheus: () => <A href="https://prometheus.io">Prometheus</A>,
  Rust      : () => <A href="https://www.rust-lang.org">Rust</A>,
  Scala     : () => <A href="https://www.scala-lang.org">Scala</A>,
  Scala3    : () => <A href="https://dotty.epfl.ch">Scala 3</A>,
  ScalaJS   : () => <A href="https://www.scala-js.org">Scala.JS</A>,
  SG        : () => <A href="https://github.com/japgolly/scala-graal">scala-graal</A>,
  ShipReq   : () => <A href="https://shipreq.com">ShipReq</A>,
  SJR       : () => <A href="https://github.com/japgolly/scalajs-react">scalajs-react</A>,
  Terraform : () => <A href="https://www.terraform.io">Terraform</A>,
  TLA       : () => <A href="https://learntla.com">TLA+</A>,
  Typescript: () => <A href="https://www.typescriptlang.org">Typescript</A>,
}
