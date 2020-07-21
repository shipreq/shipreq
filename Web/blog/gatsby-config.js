const siteMetadata = require('./src/siteMetadata');

module.exports = {
  siteMetadata,

  plugins: [

    // =============================================================================================
    "gatsby-plugin-catch-links",
    "gatsby-plugin-postcss",
    "gatsby-plugin-react-helmet",
    "gatsby-plugin-sass",
    "gatsby-plugin-sharp",
    "gatsby-plugin-sitemap",
    "gatsby-plugin-styled-components",
    "gatsby-plugin-svgr-svgo",

    // =============================================================================================
    {
      resolve: "gatsby-transformer-sharp",
      options: {
        checkSupportedExtensions: false, // Don't warn about svgs in src/images
      },
    },

    // =============================================================================================
    {
      resolve: "gatsby-plugin-mdx",
      options: {
        gatsbyRemarkPlugins: [
          "gatsby-remark-check-links",
          {
            resolve: "gatsby-remark-prismjs",
            options: {
              inlineCodeMarker: "→",
              showLineNumbers: false,
            },
          },
        ],
      },
    },

    // =============================================================================================
    {
      resolve: "gatsby-source-filesystem",
      options: {
        name: "images",
        path: "src/images/",
      },
    },
    {
      resolve: "gatsby-source-filesystem",
      options: {
        name: "posts",
        path: "src/posts/",
      },
    },

    // =============================================================================================
    // This has to come before gatsby-plugin-offline
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
    // This has to come after gatsby-plugin-manifest
    {
      resolve: "gatsby-plugin-offline",
      options: {
        workboxConfig: {
          importWorkboxFrom: "cdn",
        },
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
