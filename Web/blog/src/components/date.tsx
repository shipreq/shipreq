import { Props } from "./client-side/date"
import loadable from "@loadable/component"
import React from "react"

const fallback = <span style={{visibility: "hidden"}}>loading</span>

const LazyDate = loadable(() => import("./client-side/date"), { fallback })

export default (p: Props) => <LazyDate {...p} />
