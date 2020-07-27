import media from "styled-media-query"

export default {

  phone    : media.lessThan("small"),
  phoneWide: media.between("small", "medium"),
  tablet   : media.between("medium", "large"),
  desktop  : media.greaterThan("large"),

  small    : media.lessThan("small"),
  notSmall : media.greaterThan("small"),

  phoneAny: media.lessThan("medium"),
  notPhone: media.greaterThan("medium"),
}

/*
import R from "../utils/responsive"

  ${R.phone`
    xxxxxx;
  `}
  ${R.phoneWide`
    xxxxxx;
  `}
  ${R.tablet`
    xxxxxx;
  `}
  ${R.desktop`
    xxxxxx;
  `}
*/