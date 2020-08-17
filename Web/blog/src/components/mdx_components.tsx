import { Link } from "gatsby"
import A from "./a"
import React from "react"

const MrB = ({type}: {type: "app" | "github"}) => (
  (type == "app")
  ? <A href="https://japgolly.github.io/mr.boilerplate" >Mr. Boilerplate</A>
  : <A href="https://github.com/japgolly/mr.boilerplate">Mr. Boilerplate</A>
)

const addClass: (cls: string) => React.FC<{}> =
  cls => props => (<div className={cls}>{props.children}</div>)

export default {
  A,
  MrB,
  About    : () => <Link to="/about">About</Link>,
  BooPickle: () => <A href="https://github.com/suzaku-io/boopickle">BooPickle</A>,
  Graal    : () => <A href="https://www.graalvm.org">GraalVM</A>,
  ScalaJS  : () => <A href="https://www.scala-js.org">Scala.JS</A>,
  SG       : () => <A href="https://github.com/japgolly/scala-graal">scala-graal</A>,
  ShipReq  : () => <A href="https://shipreq.com">ShipReq</A>,
  SJR      : () => <A href="https://github.com/japgolly/scalajs-react">scalajs-react</A>,
  Katsokaa : addClass("katsokaa"),
}
