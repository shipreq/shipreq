import Typography from "typography"
import theme from "typography-theme-stern-grove"
// import theme from "typography-theme-sutro";

const customOptions = {
  includeNormalize: true,

  overrideThemeStyles: ({ rhythm }, options, styles) => ({
    "h1,h2,h3,h4,h5,h6": {
      marginTop: 0,
      marginBottom: rhythm(2),
    },
  }),
}

const typography = new Typography(Object.assign({}, theme, customOptions))

export const { scale, rhythm, options } = typography
export default typography
