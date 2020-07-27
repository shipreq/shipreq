import { localiseDate } from "../../utils/locale"
import React from "react"

export type Props = {
  date: string
  format?: string
}

export default function(p: Props) {
  const d   = localiseDate(p.date)
  const fmt = p.format || "LL"
  return <time dateTime={d.toISOString()}>{d.format(fmt)}</time>
}
