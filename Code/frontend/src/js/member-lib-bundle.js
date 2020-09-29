export { default as autosize       } from 'autosize'
export { default as ChromePicker   } from 'react-color/lib/components/chrome/Chrome'
export { default as clipboard      } from 'clipboard-polyfill'
export { default as GithubPicker   } from 'react-color/lib/components/github/Github'
export { default as HRP            } from 'html-react-parser'
export { default as moment         } from 'moment/min/moment.min.js'
export { default as ReactCollapse  } from 'react-collapse'
export { default as RVAS           } from 'react-virtualized/dist/es/AutoSizer'
export { default as scrollIntoView } from 'scroll-into-view-if-needed'
export { default as TextComplete   } from 'textcomplete/lib/textcomplete'
export { default as TextCompleteTA } from 'textcomplete/lib/textarea'
export { default as tinycolor      } from 'tinycolor2/tinycolor'

// dompurify
import DOMPurify from 'dompurify'
export { DOMPurify as DP }

// lru-cache
import LRUCache from 'lru-cache'
export { LRUCache as LRUC }

// screenfull
import * as screenfull from 'screenfull'
export { screenfull }

// text-field-edit
import * as TFE from 'text-field-edit'
export { TFE }

// react-svg-pan-zoom
import ReactSVGPanZoom from 'react-svg-pan-zoom/build-es/viewer'
import { isZoomLevelGoingOutOfBounds, limitZoomLevel } from 'react-svg-pan-zoom/build-es/features/zoom'
import { set } from 'react-svg-pan-zoom/build-es/features/common'
import {
  ACTION_ZOOM,
  ALIGN_CENTER,
  INITIAL_VALUE,
  MODE_IDLE,
  POSITION_RIGHT,
  TOOL_AUTO,
} from 'react-svg-pan-zoom/build-es/constants'
const RSPZ = {
  ReactSVGPanZoom,
  isZoomLevelGoingOutOfBounds, limitZoomLevel,
  set,
  ACTION_ZOOM,
  ALIGN_CENTER,
  INITIAL_VALUE,
  MODE_IDLE,
  POSITION_RIGHT,
  TOOL_AUTO,
}
export { RSPZ }

// transformation-matrix (for react-svg-pan-zoom)
import { scale, transform, translate } from 'transformation-matrix'
const TM = { scale, transform, translate }
export { TM }

// Paste the following into web console for quick testing.
// console.log("G: ", {autosize, ChromePicker, clipboard, GithubPicker, HRP, LRUC, moment, ReactCollapse, RSPZ, RVAS, scrollIntoView, screenfull, TextComplete, TextCompleteTA, TM, TFE, tinycolor})
