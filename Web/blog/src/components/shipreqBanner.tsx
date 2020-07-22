import React from "react"
import ShipreqBanner from "../images/shipreq-banner.inline.svg"

type Props = { [key: string]: any }

export default function(props: Props) {
  let { width, height, ...other } = props
  return <ShipreqBanner
            viewBox="0 0 1024 313.238"
            width={width || null}
            height={height || null}
            {...other}
          />
}
