export { default as autosize       } from 'autosize';
export { default as ChromePicker   } from 'react-color/lib/components/chrome/Chrome';
export { default as clipboard      } from 'clipboard-polyfill';
export { default as GithubPicker   } from 'react-color/lib/components/github/Github';
export { default as HRP            } from 'html-react-parser';
export { default as moment         } from 'moment/min/moment.min.js';
export { default as ReactCollapse  } from 'react-collapse';
export { default as RVAS           } from 'react-virtualized/dist/es/AutoSizer';
export { default as scrollIntoView } from 'scroll-into-view-if-needed';
export { default as TextComplete   } from 'textcomplete/lib/textcomplete';
export { default as TextCompleteTA } from 'textcomplete/lib/textarea';
export { default as tinycolor      } from 'tinycolor2/tinycolor';

// dompurify
import DOMPurify from 'dompurify';
export { DOMPurify as DP };

// screenfull
import * as screenfull from 'screenfull';
export { screenfull };

// react-svg-pan-zoom
import ReactSVGPanZoom from 'react-svg-pan-zoom/build-es/viewer';
import { fitToViewer } from 'react-svg-pan-zoom/build-es/features/zoom';
import {
  ALIGN_CENTER,
  INITIAL_VALUE,
  POSITION_RIGHT,
  TOOL_AUTO,
} from 'react-svg-pan-zoom/build-es/constants';
const RSPZ = {
  ReactSVGPanZoom,
  fitToViewer,
  ALIGN_CENTER,
  INITIAL_VALUE,
  POSITION_RIGHT,
  TOOL_AUTO,
};
export { RSPZ };

// Paste the following into web console for quick testing.
// console.log("G: ", {autosize, ChromePicker, clipboard, GithubPicker, HRP, moment, ReactCollapse, RSPZ, RVAS, scrollIntoView, screenfull, TextComplete, TextCompleteTA, tinycolor})
