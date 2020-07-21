const siteMetadata = require('./src/siteMetadata');

module.exports = {
  siteMetadata,

  plugins: [

    // =============================================================================================
    "gatsby-plugin-catch-links",
    "gatsby-plugin-svgr-svgo",

    // =============================================================================================
    {
      resolve: "gatsby-plugin-mdx",
      options: {
        gatsbyRemarkPlugins: [
          "gatsby-remark-check-links",
        ],
      },
    },

    // =============================================================================================
    {
      resolve: "gatsby-source-filesystem",
      options: {
        name: "posts",
        path: "src/posts/",
      },
    },

    // =============================================================================================
    {
      resolve: "gatsby-plugin-manifest",
      options: {
        name            : "ShipReq tech blog",
        start_url       : "/",
        display         : "browser",
        icon            : "src/images/favicon.svg",
        crossOrigin     : "use-credentials",
        background_color: "#ffffff",
        theme_color     : "#ffffff",
      },
    },

    // =============================================================================================
    {
      resolve: "gatsby-plugin-typography",
      options: {
        pathToConfigModule: "src/utils/typography",
      },
    },

  ],
}
