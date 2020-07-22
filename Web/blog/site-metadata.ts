function singleLine(l: string) {
  return l.replace(/\s+/g, " ").trim()
}

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

  author: {
    twitterHandle: "@japgolly",
  }
};
