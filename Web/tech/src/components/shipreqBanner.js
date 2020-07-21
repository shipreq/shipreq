import React from "react"
import ShipreqBanner from "../images/shipreq-banner.inline.svg"

export default function(props) {
  let { width, height, ...other } = props
  return <ShipreqBanner
            viewBox="0 0 1024 313.238"
            width={width || null}
            height={height || null}
            {...other}
          />
}
