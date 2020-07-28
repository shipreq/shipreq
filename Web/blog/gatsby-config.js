require("ts-node").register({ files: true })

const siteMetadata = require("./src/config/site").default
const routes = require("./src/utils/routes")
const { isProd } = siteMetadata

module.exports = {
  siteMetadata,

  plugins: [
    // =============================================================================================
    "gatsby-plugin-catch-links",
    "gatsby-plugin-lodash",
    "gatsby-plugin-postcss",
    "gatsby-plugin-react-helmet",
    "gatsby-plugin-sass",
    "gatsby-plugin-sharp",
    "gatsby-plugin-sitemap",
    "gatsby-plugin-styled-components",
    "gatsby-plugin-svgr-svgo",

    // =============================================================================================
    "gatsby-plugin-typescript",
    "gatsby-plugin-typescript-checker",

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
          // "gatsby-remark-autolink-headers",
          "gatsby-remark-check-links",
          "gatsby-remark-smartypants",
          {
            resolve: "gatsby-remark-external-links",
            options: {
              rel: "noopener",
            },
          },
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
        name: "posts",
        path: "content/posts/",
      },
    },
    {
      resolve: "gatsby-source-filesystem",
      options: {
        name: "images",
        path: "src/images/",
      },
    },

    // =============================================================================================
    // This has to come before gatsby-plugin-offline
    {
      resolve: "gatsby-plugin-manifest",
      options: {
        name            : siteMetadata.title,
        start_url       : "/",
        display         : "browser",
        icon            : "src/images/favicon.svg",
        crossOrigin     : "use-credentials",
        background_color: "#001a30",
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

    // =============================================================================================
    {
      resolve: `gatsby-plugin-feed`,
      options: {
        feeds: [
          {
            query: `
              {
                allMdx(
                  sort: {fields: [frontmatter___date], order: DESC},
                  filter: {fileAbsolutePath: {glob: "**/posts/*"}}
                ) {
                  edges {
                    node {
                      ...PostNode
                    }
                  }
                }
              }
            `,
            serialize: ({ query: { allMdx } }) => {
              return allMdx.edges.map(edge => {
                const n = edge.node
                return Object.assign({}, {
                  title      : n.frontmatter.title,
                  description: n.frontmatter.desc,
                  date       : n.frontmatter.date,
                  url        : routes.urlForPost(n),
                })
              })
            },
            output: siteMetadata.rssPath,
            title: siteMetadata.title,
          },
        ],
      },
    },

  ].filter(o => !!o),
}
