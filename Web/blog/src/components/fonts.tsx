import React from "react"
import { Helmet } from "react-helmet"

const all = [
  // 'family=Nunito:wght@700',
  'family=Nunito+Sans:ital,wght@0,400;0,600;0,700;1,400;1,700',
]

export default () => (<Helmet>

  <link href={`https://fonts.googleapis.com/css2?${all.join("&")}&display=swap`} rel="stylesheet" />

</Helmet>)
