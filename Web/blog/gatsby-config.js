require("ts-node").register({ files: true })

const siteMetadata = require("./src/config/site").default
const routes = require("./src/utils/routes")

const compressExts = ['css', 'html', 'js', 'json', 'svg', 'xml']

module.exports = {
  siteMetadata,

  plugins: [
    // =============================================================================================
    "gatsby-plugin-catch-links",
    "gatsby-plugin-lodash",
    "gatsby-plugin-postcss",
    "gatsby-plugin-react-helmet",
    "gatsby-plugin-remove-serviceworker",
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
          "gatsby-remark-graphviz", // has to come before gatsby-remark-prismjs
          "gatsby-remark-smartypants",
          {
            resolve: "gatsby-remark-external-links",
            options: {
              rel: "noopener",
            },
          },
          {
            resolve: "gatsby-remark-prismjs", // has to come after gatsby-remark-graphviz
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
        name  : "pages",
        path  : "content/pages/",
        ignore: process.env.NODE_ENV === `production` && [`**/draft-*`]
      },
    },
    {
      resolve: "gatsby-source-filesystem",
      options: {
        name  : "posts",
        path  : "content/posts/",
        ignore: process.env.NODE_ENV === `production` && [`**/draft-*`]
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
    {
      resolve: "gatsby-plugin-manifest",
      options: {
        name            : siteMetadata.title,
        short_name      : siteMetadata.title,
        start_url       : "/",
        display         : "minimal-ui",
        lang            : "en",
        crossOrigin     : "use-credentials",
        background_color: "#ffffff",
        theme_color     : "#001a30",
        icon            : "src/images/favicon.svg",
        icon_options    : {
          purpose: "any maskable",
        },
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
                      id
                      frontmatter {
                        date
                        desc
                        slug
                        tags
                        title
                      }
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

    // =============================================================================================
    (process.env.NODE_ENV === `production`) && {
      resolve: 'gatsby-plugin-brotli',
      options: {
        extensions: compressExts,
      },
    },

    // =============================================================================================
    (process.env.NODE_ENV === `production`) && {
      resolve: 'gatsby-plugin-zopfli',
      options: {
        extensions: compressExts,
      },
    },

    // =============================================================================================
    {
      resolve: "gatsby-plugin-s3",
      options: {
        bucketName: siteMetadata.s3.bucket,
        region    : siteMetadata.s3.region,
        protocol  : 'https',
        hostname  : siteMetadata.hostname,
      },
    },

  ].filter(o => !!o),
}
