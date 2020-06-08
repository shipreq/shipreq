import 'core-js/features/map';
import 'core-js/features/object/assign';
import 'core-js/features/set';

window.React          = require('react');
window.ReactDOM       = require('react-dom');
window.ReactDOMServer = require('react-dom/server');
window.ReactTestUtils = require('react-dom/test-utils');

window.autosize       = require('autosize');
window.ChromePicker   = require('react-color/lib/components/chrome/Chrome');
window.GithubPicker   = require('react-color/lib/components/github/Github');
window.moment         = require('moment');
window.ReactCollapse  = require('react-collapse');
window.RSPZ           = require('react-svg-pan-zoom/build-umd/react-svg-pan-zoom')
window.scrollIntoView = require('scroll-into-view-if-needed');
window.TextComplete   = require('textcomplete/lib/textcomplete');
window.TextCompleteTA = require('textcomplete/lib/textarea');
window.tinycolor      = require('tinycolor2/tinycolor');

import htmlReactParser from 'html-react-parser';
window.HRP = htmlReactParser;

import AutoSizer from 'react-virtualized/dist/es/AutoSizer';
window.RVAS = AutoSizer;
