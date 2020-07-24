import { GoogleAnalytics, StatCounter } from "./types"

const isProd: boolean = /prod/.test("" + process.env.ENV)
const isDev : boolean = !isProd

if (isProd)
  console.log("\u001b[41;1;37;1mUsing Production config\u001b[0m")
else
  console.log("\u001b[43;30mUsing Development config\u001b[0m")

function singleLine(l: string) {
  return l.replace(/\s+/g, " ").trim()
}

const googleAnalytics: GoogleAnalytics | null =
  isDev ?
  {
    trackingId: "UA-173267009-2",
    jsUrl     : "http://localhost:3000/*(d3d3Lmdvb2dsZXRhZ21hbmFnZXIuY29t)*/*(Z3RhZw)*/*(anM%2FaWQ9VUEtMTczMjY3MDA5LTI)*",
    disabled  : true,
  } : {
    trackingId: "UA-105581783-3",
    jsUrl     : "https://ap.shipreq.com/*(d3d3Lmdvb2dsZXRhZ21hbmFnZXIuY29t)*/*(Z3RhZw)*/*(anM%2FaWQ9VUEtMTA1NTgxNzgzLTM)*",
  }

const statCounter: StatCounter | null =
  isDev ?
  {
    project : 12363376,
    security: "bec10e58",
    https   : false,
    jsUrl   : "http://localhost:3000/*(d3d3LnN0YXRjb3VudGVyLmNvbQ)*/*(Y291bnRlcg)*/*(Y291bnRlci5qcw)*",
    disabled: true,
  } : {
    project : 12363377,
    security: "e1f055a7",
    https   : true,
    jsUrl   : "https://ap.shipreq.com/*(d3d3LnN0YXRjb3VudGVyLmNvbQ)*/*(Y291bnRlcg)*/*(Y291bnRlci5qcw)*",
  }

const Year = new Date().getFullYear()

export default {

  title: "ShipReq Blog",

  siteUrl: "https://blog.shipreq.com",

  cardImageUrl: "https://blog.shipreq.com/static/ab7f7f3a4df0120151227c3bbb6eed6e/logo-title-1024.png",

  copyright1: `© 2013-${Year} Bearded Logic.`,
  copyright2: "All rights reserved.",

  locale: "en_AU",

  description: singleLine(`
    A blog detailing technical learnings and experiences during work on ShipReq.
    Common themes are FP (functional programming) with Scala, Scala.JS, @japgolly libraries
    like scalajs-react, scalacss, scala-graal, and other related technologies.
    Coding with high quality is an important ShipReq value and so you can also expect articles
    about advanced type-safety, testing, performance/benchmarking.
    ShipReq is a modern, online tool for software requirements development and management.
  `),

  rssPath: "/rss.xml",

  analytics: {
    googleAnalytics,
    statCounter,
  },

  email: {
    address: "contact@shipreq.com",
    mailto: "mailto:contact@shipreq.com",
  },

  reddit: {
    url: "https://www.reddit.com/r/shipreq/",
  },

  twitter: {
    handle: "@shipreq",
    url: "https://twitter.com/shipreq",
  },

  japgolly: {
    name: "David Barri",
    bio: singleLine(`
      Hi! I'm the founder/creator of ShipReq.
      I've been coding since I was a kid, and
      have been euphorically doing functional programming in Scala for ${Year - 2012} years and counting.
      I love to create that which sparks joy in others.
    `),
    link: "https://github.com/japgolly",
    twitter: {
      handle: "@japgolly",
      url: "https://twitter.com/japgolly",
    },
  },

};
