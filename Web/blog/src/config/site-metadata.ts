import { StatCounter } from "./types"

const isProd: boolean = /prod/.test("" + process.env.ENV)
const isDev : boolean = !isProd

if (isProd)
  console.log("\u001b[41;1;37;1mUsing Production config\u001b[0m")
else
  console.log("\u001b[43;30mUsing Development config\u001b[0m")

function singleLine(l: string) {
  return l.replace(/\s+/g, " ").trim()
}

const apProxy           = "https://ap.shipreq.com"
const statCounterJsPath = "www.statcounter.com/counter/counter.js"

const statCounter: StatCounter | null =
  isDev ?
  {project: 12363376, security: "bec10e58", https: false, jsUrl: `https://${statCounterJsPath}`, disabled: true} :
  {project: 12363377, security: "e1f055a7", https: true , jsUrl: `${apProxy}/${statCounterJsPath}`}

module.exports = {

  title: "ShipReq Blog",

  siteUrl: "https://blog.shipreq.com",

  cardImageUrl: "https://blog.shipreq.com/qwe.png",

  locale: "en_AU",

  twitterHandle: "@shipreq",

  description: singleLine(`
    A blog detailing technical learnings and experiences during work on ShipReq.
    Common themes are FP (functional programming) with Scala, Scala.JS, @japgolly libraries
    like scalajs-react, scalacss, scala-graal, and other related technologies.
    Coding with high quality is an important ShipReq value and so you can also expect articles
    about advanced type-safety, testing, performance/benchmarking.
    ShipReq is a modern, online tool for software requirements development and management.
  `),

  analytics: {
    statCounter,
  },

  author: {
    twitterHandle: "@japgolly",
  }
};
