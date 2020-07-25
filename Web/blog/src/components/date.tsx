import { Props } from "./client-side/date"
import loadable from "@loadable/component"
import React from "react"

const LazyDate = loadable(() => import("./client-side/date"))

export default (p: Props) => <LazyDate {...p} />
