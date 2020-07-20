/**
 * Configure your Gatsby site with this file.
 *
 * See: https://www.gatsbyjs.org/docs/gatsby-config/
 */

 const manifest = {
  resolve: `gatsby-plugin-manifest`,
  options: {
    name: "ShipReq tech blog",
    start_url: "/",
    background_color: "#ffffff",
    theme_color: "#ffffff",
    display: "browser",
    icon: "src/images/shipreq-logo-only.svg",
    crossOrigin: `use-credentials`,
  },
};

const typography = {
  resolve: `gatsby-plugin-typography`,
  options: {
    pathToConfigModule: `src/utils/typography`,
  },
};

module.exports = {
  plugins: [
    manifest,
    typography,
  ],
}
