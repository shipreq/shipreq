!function(){"use strict"
const r=self||window
r.VizModule={locateFile:function(){return vizWasmFile}},r.Viz={},function(r){void 0!==r.Viz&&(r.Viz.render=function(r,e,n){var t
for(t=0;t<n.files.length;t++)r.ccall("vizCreateFile","number",["string","string"],[n.files[t].path,n.files[t].data])
r.ccall("vizSetY_invert","number",["number"],[n.yInvert?1:0]),r.ccall("vizSetNop","number",["number"],[n.nop?n.nop:0])
var o=r.ccall("vizRenderFromString","number",["string","string","string"],[e,n.format,n.engine]),i=r.UTF8ToString(o)
r.ccall("free","number",["number"],[o])
var a=r.ccall("vizLastErrorMessage","number",[],[]),s=r.UTF8ToString(a)
if(r.ccall("free","number",["number"],[a]),""!=s)throw new Error(s)
return i},r.Viz.Module=function(r){r=r||{}
r="undefined"!=typeof VizModule?VizModule:{}
var e,n={}
for(e in r)r.hasOwnProperty(e)&&(n[e]=r[e])
var t,o,i="./this.program",a=function(r,e){throw e}
t="object"==typeof window,o="function"==typeof importScripts,"object"==typeof process&&"object"==typeof process.versions&&process.versions.node
var s,u,c="";(t||o)&&(o?c=self.location.href:document.currentScript&&(c=document.currentScript.src),c=0!==c.indexOf("blob:")?c.substr(0,c.lastIndexOf("/")+1):"",s=function(r){var e=new XMLHttpRequest
return e.open("GET",r,!1),e.send(null),e.responseText},o&&(u=function(r){var e=new XMLHttpRequest
return e.open("GET",r,!1),e.responseType="arraybuffer",e.send(null),new Uint8Array(e.response)}))
var l=r.print||console.log.bind(console),f=r.printErr||console.warn.bind(console)
for(e in n)n.hasOwnProperty(e)&&(r[e]=n[e])
n=null,r.arguments&&r.arguments,r.thisProgram&&(i=r.thisProgram),r.quit&&(a=r.quit)
var d,m,p,h=0
r.wasmBinary&&(d=r.wasmBinary),r.noExitRuntime&&(m=r.noExitRuntime),"object"!=typeof WebAssembly&&X("no native wasm support detected")
var v=new WebAssembly.Table({initial:425,maximum:425,element:"anyfunc"}),w=!1
function g(r,e){r||X("Assertion failed: "+e)}var y="undefined"!=typeof TextDecoder?new TextDecoder("utf8"):void 0
function E(r,e,n){for(var t=e+n,o=e;r[o]&&!(o>=t);)++o
if(o-e>16&&r.subarray&&y)return y.decode(r.subarray(e,o))
for(var i="";e<o;){var a=r[e++]
if(128&a){var s=63&r[e++]
if(192!=(224&a)){var u=63&r[e++]
if((a=224==(240&a)?(15&a)<<12|s<<6|u:(7&a)<<18|s<<12|u<<6|63&r[e++])<65536)i+=String.fromCharCode(a)
else{var c=a-65536
i+=String.fromCharCode(55296|c>>10,56320|1023&c)}}else i+=String.fromCharCode((31&a)<<6|s)}else i+=String.fromCharCode(a)}return i}function _(r,e){return r?E(P,r,e):""}function k(r,e,n,t){if(!(t>0))return 0
for(var o=n,i=n+t-1,a=0;a<r.length;++a){var s=r.charCodeAt(a)
if(s>=55296&&s<=57343)s=65536+((1023&s)<<10)|1023&r.charCodeAt(++a)
if(s<=127){if(n>=i)break
e[n++]=s}else if(s<=2047){if(n+1>=i)break
e[n++]=192|s>>6,e[n++]=128|63&s}else if(s<=65535){if(n+2>=i)break
e[n++]=224|s>>12,e[n++]=128|s>>6&63,e[n++]=128|63&s}else{if(n+3>=i)break
e[n++]=240|s>>18,e[n++]=128|s>>12&63,e[n++]=128|s>>6&63,e[n++]=128|63&s}}return e[n]=0,n-o}function b(r,e,n){return k(r,P,e,n)}function D(r){for(var e=0,n=0;n<r.length;++n){var t=r.charCodeAt(n)
t>=55296&&t<=57343&&(t=65536+((1023&t)<<10)|1023&r.charCodeAt(++n)),t<=127?++e:e+=t<=2047?2:t<=65535?3:4}return e}var S,F,P,A,x,M
function R(e){S=e,r.HEAP8=F=new Int8Array(e),r.HEAP16=A=new Int16Array(e),r.HEAP32=x=new Int32Array(e),r.HEAPU8=P=new Uint8Array(e),r.HEAPU16=new Uint16Array(e),r.HEAPU32=new Uint32Array(e),r.HEAPF32=new Float32Array(e),r.HEAPF64=M=new Float64Array(e)}var z=r.INITIAL_MEMORY||16777216
function C(e){for(;e.length>0;){var n=e.shift()
if("function"!=typeof n){var t=n.func
"number"==typeof t?void 0===n.arg?r.dynCall_v(t):r.dynCall_vi(t,n.arg):t(void 0===n.arg?null:n.arg)}else n(r)}}(p=r.wasmMemory?r.wasmMemory:new WebAssembly.Memory({initial:z/65536,maximum:32768}))&&(S=p.buffer),z=S.byteLength,R(S),x[50656]=5445664
var T=[],j=[],B=[],N=[]
var O=Math.abs,L=Math.ceil,I=Math.floor,U=Math.min,H=0,W=null
function q(e){H++,r.monitorRunDependencies&&r.monitorRunDependencies(H)}function V(e){if(H--,r.monitorRunDependencies&&r.monitorRunDependencies(H),0==H&&W){var n=W
W=null,n()}}function X(e){throw r.onAbort&&r.onAbort(e),l(e+=""),f(e),w=!0,e="abort("+e+"). Build with -s ASSERTIONS=1 for more info.",new WebAssembly.RuntimeError(e)}r.preloadedImages={},r.preloadedAudios={}
function G(r){return e=r,n="data:application/octet-stream;base64,",String.prototype.startsWith?e.startsWith(n):0===e.indexOf(n)
var e,n}var K,Y,Z,$=""
function J(){try{if(d)return new Uint8Array(d)
if(u)return u($)
throw"both async and sync fetching of the wasm failed"}catch(r){X(r)}}G($)||(K=$,$=r.locateFile?r.locateFile(K,c):c+K)
var Q,rr={1024:function(r,e){var n=_(r),t=_(e)
sr.createPath("/",tr.dirname(n)),sr.writeFile(tr.join("/",n),t)}}
function er(){var e=function(){var r=new Error
if(!r.stack){try{throw new Error}catch(e){r=e}if(!r.stack)return"(no stack trace available)"}return r.stack.toString()}()
return r.extraStackTrace&&(e+="\n"+r.extraStackTrace()),e.replace(/\b_Z[\w\d_]+/g,(function(r){return r==r?r:r+" ["+r+"]"}))}j.push({func:function(){wr()}}),Q=function(){return performance.now()}
function nr(r){return x[Er()>>2]=r,r}var tr={splitPath:function(r){return/^(\/?|)([\s\S]*?)((?:\.{1,2}|[^\/]+?|)(\.[^.\/]*|))(?:[\/]*)$/.exec(r).slice(1)},normalizeArray:function(r,e){for(var n=0,t=r.length-1;t>=0;t--){var o=r[t]
"."===o?r.splice(t,1):".."===o?(r.splice(t,1),n++):n&&(r.splice(t,1),n--)}if(e)for(;n;n--)r.unshift("..")
return r},normalize:function(r){var e="/"===r.charAt(0),n="/"===r.substr(-1)
return(r=tr.normalizeArray(r.split("/").filter((function(r){return!!r})),!e).join("/"))||e||(r="."),r&&n&&(r+="/"),(e?"/":"")+r},dirname:function(r){var e=tr.splitPath(r),n=e[0],t=e[1]
return n||t?(t&&(t=t.substr(0,t.length-1)),n+t):"."},basename:function(r){if("/"===r)return"/"
var e=r.lastIndexOf("/")
return-1===e?r:r.substr(e+1)},extname:function(r){return tr.splitPath(r)[3]},join:function(){var r=Array.prototype.slice.call(arguments,0)
return tr.normalize(r.join("/"))},join2:function(r,e){return tr.normalize(r+"/"+e)}},or={resolve:function(){for(var r="",e=!1,n=arguments.length-1;n>=-1&&!e;n--){var t=n>=0?arguments[n]:sr.cwd()
if("string"!=typeof t)throw new TypeError("Arguments to path.resolve must be strings")
if(!t)return""
r=t+"/"+r,e="/"===t.charAt(0)}return(e?"/":"")+(r=tr.normalizeArray(r.split("/").filter((function(r){return!!r})),!e).join("/"))||"."},relative:function(r,e){function n(r){for(var e=0;e<r.length&&""===r[e];e++);for(var n=r.length-1;n>=0&&""===r[n];n--);return e>n?[]:r.slice(e,n-e+1)}r=or.resolve(r).substr(1),e=or.resolve(e).substr(1)
for(var t=n(r.split("/")),o=n(e.split("/")),i=Math.min(t.length,o.length),a=i,s=0;s<i;s++)if(t[s]!==o[s]){a=s
break}var u=[]
for(s=a;s<t.length;s++)u.push("..")
return(u=u.concat(o.slice(a))).join("/")}},ir={ttys:[],init:function(){},shutdown:function(){},register:function(r,e){ir.ttys[r]={input:[],output:[],ops:e},sr.registerDevice(r,ir.stream_ops)},stream_ops:{open:function(r){var e=ir.ttys[r.node.rdev]
if(!e)throw new sr.ErrnoError(43)
r.tty=e,r.seekable=!1},close:function(r){r.tty.ops.flush(r.tty)},flush:function(r){r.tty.ops.flush(r.tty)},read:function(r,e,n,t,o){if(!r.tty||!r.tty.ops.get_char)throw new sr.ErrnoError(60)
for(var i=0,a=0;a<t;a++){var s
try{s=r.tty.ops.get_char(r.tty)}catch(r){throw new sr.ErrnoError(29)}if(void 0===s&&0===i)throw new sr.ErrnoError(6)
if(null==s)break
i++,e[n+a]=s}return i&&(r.node.timestamp=Date.now()),i},write:function(r,e,n,t,o){if(!r.tty||!r.tty.ops.put_char)throw new sr.ErrnoError(60)
try{for(var i=0;i<t;i++)r.tty.ops.put_char(r.tty,e[n+i])}catch(r){throw new sr.ErrnoError(29)}return t&&(r.node.timestamp=Date.now()),i}},default_tty_ops:{get_char:function(r){if(!r.input.length){var e=null
if("undefined"!=typeof window&&"function"==typeof window.prompt?null!==(e=window.prompt("Input: "))&&(e+="\n"):"function"==typeof readline&&null!==(e=readline())&&(e+="\n"),!e)return null
r.input=pr(e,!0)}return r.input.shift()},put_char:function(r,e){null===e||10===e?(l(E(r.output,0)),r.output=[]):0!=e&&r.output.push(e)},flush:function(r){r.output&&r.output.length>0&&(l(E(r.output,0)),r.output=[])}},default_tty1_ops:{put_char:function(r,e){null===e||10===e?(f(E(r.output,0)),r.output=[]):0!=e&&r.output.push(e)},flush:function(r){r.output&&r.output.length>0&&(f(E(r.output,0)),r.output=[])}}},ar={ops_table:null,mount:function(r){return ar.createNode(null,"/",16895,0)},createNode:function(r,e,n,t){if(sr.isBlkdev(n)||sr.isFIFO(n))throw new sr.ErrnoError(63)
ar.ops_table||(ar.ops_table={dir:{node:{getattr:ar.node_ops.getattr,setattr:ar.node_ops.setattr,lookup:ar.node_ops.lookup,mknod:ar.node_ops.mknod,rename:ar.node_ops.rename,unlink:ar.node_ops.unlink,rmdir:ar.node_ops.rmdir,readdir:ar.node_ops.readdir,symlink:ar.node_ops.symlink},stream:{llseek:ar.stream_ops.llseek}},file:{node:{getattr:ar.node_ops.getattr,setattr:ar.node_ops.setattr},stream:{llseek:ar.stream_ops.llseek,read:ar.stream_ops.read,write:ar.stream_ops.write,allocate:ar.stream_ops.allocate,mmap:ar.stream_ops.mmap,msync:ar.stream_ops.msync}},link:{node:{getattr:ar.node_ops.getattr,setattr:ar.node_ops.setattr,readlink:ar.node_ops.readlink},stream:{}},chrdev:{node:{getattr:ar.node_ops.getattr,setattr:ar.node_ops.setattr},stream:sr.chrdev_stream_ops}})
var o=sr.createNode(r,e,n,t)
return sr.isDir(o.mode)?(o.node_ops=ar.ops_table.dir.node,o.stream_ops=ar.ops_table.dir.stream,o.contents={}):sr.isFile(o.mode)?(o.node_ops=ar.ops_table.file.node,o.stream_ops=ar.ops_table.file.stream,o.usedBytes=0,o.contents=null):sr.isLink(o.mode)?(o.node_ops=ar.ops_table.link.node,o.stream_ops=ar.ops_table.link.stream):sr.isChrdev(o.mode)&&(o.node_ops=ar.ops_table.chrdev.node,o.stream_ops=ar.ops_table.chrdev.stream),o.timestamp=Date.now(),r&&(r.contents[e]=o),o},getFileDataAsRegularArray:function(r){if(r.contents&&r.contents.subarray){for(var e=[],n=0;n<r.usedBytes;++n)e.push(r.contents[n])
return e}return r.contents},getFileDataAsTypedArray:function(r){return r.contents?r.contents.subarray?r.contents.subarray(0,r.usedBytes):new Uint8Array(r.contents):new Uint8Array(0)},expandFileStorage:function(r,e){var n=r.contents?r.contents.length:0
if(!(n>=e)){e=Math.max(e,n*(n<1048576?2:1.125)>>>0),0!=n&&(e=Math.max(e,256))
var t=r.contents
r.contents=new Uint8Array(e),r.usedBytes>0&&r.contents.set(t.subarray(0,r.usedBytes),0)}},resizeFileStorage:function(r,e){if(r.usedBytes!=e){if(0==e)return r.contents=null,void(r.usedBytes=0)
if(!r.contents||r.contents.subarray){var n=r.contents
return r.contents=new Uint8Array(e),n&&r.contents.set(n.subarray(0,Math.min(e,r.usedBytes))),void(r.usedBytes=e)}if(r.contents||(r.contents=[]),r.contents.length>e)r.contents.length=e
else for(;r.contents.length<e;)r.contents.push(0)
r.usedBytes=e}},node_ops:{getattr:function(r){var e={}
return e.dev=sr.isChrdev(r.mode)?r.id:1,e.ino=r.id,e.mode=r.mode,e.nlink=1,e.uid=0,e.gid=0,e.rdev=r.rdev,sr.isDir(r.mode)?e.size=4096:sr.isFile(r.mode)?e.size=r.usedBytes:sr.isLink(r.mode)?e.size=r.link.length:e.size=0,e.atime=new Date(r.timestamp),e.mtime=new Date(r.timestamp),e.ctime=new Date(r.timestamp),e.blksize=4096,e.blocks=Math.ceil(e.size/e.blksize),e},setattr:function(r,e){void 0!==e.mode&&(r.mode=e.mode),void 0!==e.timestamp&&(r.timestamp=e.timestamp),void 0!==e.size&&ar.resizeFileStorage(r,e.size)},lookup:function(r,e){throw sr.genericErrors[44]},mknod:function(r,e,n,t){return ar.createNode(r,e,n,t)},rename:function(r,e,n){if(sr.isDir(r.mode)){var t
try{t=sr.lookupNode(e,n)}catch(r){}if(t)for(var o in t.contents)throw new sr.ErrnoError(55)}delete r.parent.contents[r.name],r.name=n,e.contents[n]=r,r.parent=e},unlink:function(r,e){delete r.contents[e]},rmdir:function(r,e){var n=sr.lookupNode(r,e)
for(var t in n.contents)throw new sr.ErrnoError(55)
delete r.contents[e]},readdir:function(r){var e=[".",".."]
for(var n in r.contents)r.contents.hasOwnProperty(n)&&e.push(n)
return e},symlink:function(r,e,n){var t=ar.createNode(r,e,41471,0)
return t.link=n,t},readlink:function(r){if(!sr.isLink(r.mode))throw new sr.ErrnoError(28)
return r.link}},stream_ops:{read:function(r,e,n,t,o){var i=r.node.contents
if(o>=r.node.usedBytes)return 0
var a=Math.min(r.node.usedBytes-o,t)
if(a>8&&i.subarray)e.set(i.subarray(o,o+a),n)
else for(var s=0;s<a;s++)e[n+s]=i[o+s]
return a},write:function(r,e,n,t,o,i){if(e.buffer===F.buffer&&(i=!1),!t)return 0
var a=r.node
if(a.timestamp=Date.now(),e.subarray&&(!a.contents||a.contents.subarray)){if(i)return a.contents=e.subarray(n,n+t),a.usedBytes=t,t
if(0===a.usedBytes&&0===o)return a.contents=e.slice(n,n+t),a.usedBytes=t,t
if(o+t<=a.usedBytes)return a.contents.set(e.subarray(n,n+t),o),t}if(ar.expandFileStorage(a,o+t),a.contents.subarray&&e.subarray)a.contents.set(e.subarray(n,n+t),o)
else for(var s=0;s<t;s++)a.contents[o+s]=e[n+s]
return a.usedBytes=Math.max(a.usedBytes,o+t),t},llseek:function(r,e,n){var t=e
if(1===n?t+=r.position:2===n&&sr.isFile(r.node.mode)&&(t+=r.node.usedBytes),t<0)throw new sr.ErrnoError(28)
return t},allocate:function(r,e,n){ar.expandFileStorage(r.node,e+n),r.node.usedBytes=Math.max(r.node.usedBytes,e+n)},mmap:function(r,e,n,t,o,i){if(g(0===e),!sr.isFile(r.node.mode))throw new sr.ErrnoError(43)
var a,s,u=r.node.contents
if(2&i||u.buffer!==S){if((t>0||t+n<u.length)&&(u=u.subarray?u.subarray(t,t+n):Array.prototype.slice.call(u,t,t+n)),s=!0,!(a=gr(n)))throw new sr.ErrnoError(48)
F.set(u,a)}else s=!1,a=u.byteOffset
return{ptr:a,allocated:s}},msync:function(r,e,n,t,o){if(!sr.isFile(r.node.mode))throw new sr.ErrnoError(43)
if(2&o)return 0
ar.stream_ops.write(r,e,0,t,n,!1)
return 0}}},sr={root:null,mounts:[],devices:{},streams:[],nextInode:1,nameTable:null,currentPath:"/",initialized:!1,ignorePermissions:!0,trackingDelegate:{},tracking:{openFlags:{READ:1,WRITE:2}},ErrnoError:null,genericErrors:{},filesystems:null,syncFSRequests:0,handleFSError:function(r){if(!(r instanceof sr.ErrnoError))throw r+" : "+er()
return nr(r.errno)},lookupPath:function(r,e){if(e=e||{},!(r=or.resolve(sr.cwd(),r)))return{path:"",node:null}
var n={follow_mount:!0,recurse_count:0}
for(var t in n)void 0===e[t]&&(e[t]=n[t])
if(e.recurse_count>8)throw new sr.ErrnoError(32)
for(var o=tr.normalizeArray(r.split("/").filter((function(r){return!!r})),!1),i=sr.root,a="/",s=0;s<o.length;s++){var u=s===o.length-1
if(u&&e.parent)break
if(i=sr.lookupNode(i,o[s]),a=tr.join2(a,o[s]),sr.isMountpoint(i)&&(!u||u&&e.follow_mount)&&(i=i.mounted.root),!u||e.follow)for(var c=0;sr.isLink(i.mode);){var l=sr.readlink(a)
if(a=or.resolve(tr.dirname(a),l),i=sr.lookupPath(a,{recurse_count:e.recurse_count}).node,c++>40)throw new sr.ErrnoError(32)}}return{path:a,node:i}},getPath:function(r){for(var e;;){if(sr.isRoot(r)){var n=r.mount.mountpoint
return e?"/"!==n[n.length-1]?n+"/"+e:n+e:n}e=e?r.name+"/"+e:r.name,r=r.parent}},hashName:function(r,e){for(var n=0,t=0;t<e.length;t++)n=(n<<5)-n+e.charCodeAt(t)|0
return(r+n>>>0)%sr.nameTable.length},hashAddNode:function(r){var e=sr.hashName(r.parent.id,r.name)
r.name_next=sr.nameTable[e],sr.nameTable[e]=r},hashRemoveNode:function(r){var e=sr.hashName(r.parent.id,r.name)
if(sr.nameTable[e]===r)sr.nameTable[e]=r.name_next
else for(var n=sr.nameTable[e];n;){if(n.name_next===r){n.name_next=r.name_next
break}n=n.name_next}},lookupNode:function(r,e){var n=sr.mayLookup(r)
if(n)throw new sr.ErrnoError(n,r)
for(var t=sr.hashName(r.id,e),o=sr.nameTable[t];o;o=o.name_next){var i=o.name
if(o.parent.id===r.id&&i===e)return o}return sr.lookup(r,e)},createNode:function(r,e,n,t){var o=new sr.FSNode(r,e,n,t)
return sr.hashAddNode(o),o},destroyNode:function(r){sr.hashRemoveNode(r)},isRoot:function(r){return r===r.parent},isMountpoint:function(r){return!!r.mounted},isFile:function(r){return 32768==(61440&r)},isDir:function(r){return 16384==(61440&r)},isLink:function(r){return 40960==(61440&r)},isChrdev:function(r){return 8192==(61440&r)},isBlkdev:function(r){return 24576==(61440&r)},isFIFO:function(r){return 4096==(61440&r)},isSocket:function(r){return 49152==(49152&r)},flagModes:{r:0,rs:1052672,"r+":2,w:577,wx:705,xw:705,"w+":578,"wx+":706,"xw+":706,a:1089,ax:1217,xa:1217,"a+":1090,"ax+":1218,"xa+":1218},modeStringToFlags:function(r){var e=sr.flagModes[r]
if(void 0===e)throw new Error("Unknown file open mode: "+r)
return e},flagsToPermissionString:function(r){var e=["r","w","rw"][3&r]
return 512&r&&(e+="w"),e},nodePermissions:function(r,e){return sr.ignorePermissions||(-1===e.indexOf("r")||292&r.mode)&&(-1===e.indexOf("w")||146&r.mode)&&(-1===e.indexOf("x")||73&r.mode)?0:2},mayLookup:function(r){var e=sr.nodePermissions(r,"x")
return e||(r.node_ops.lookup?0:2)},mayCreate:function(r,e){try{sr.lookupNode(r,e)
return 20}catch(r){}return sr.nodePermissions(r,"wx")},mayDelete:function(r,e,n){var t
try{t=sr.lookupNode(r,e)}catch(r){return r.errno}var o=sr.nodePermissions(r,"wx")
if(o)return o
if(n){if(!sr.isDir(t.mode))return 54
if(sr.isRoot(t)||sr.getPath(t)===sr.cwd())return 10}else if(sr.isDir(t.mode))return 31
return 0},mayOpen:function(r,e){return r?sr.isLink(r.mode)?32:sr.isDir(r.mode)&&("r"!==sr.flagsToPermissionString(e)||512&e)?31:sr.nodePermissions(r,sr.flagsToPermissionString(e)):44},MAX_OPEN_FDS:4096,nextfd:function(r,e){r=r||0,e=e||sr.MAX_OPEN_FDS
for(var n=r;n<=e;n++)if(!sr.streams[n])return n
throw new sr.ErrnoError(33)},getStream:function(r){return sr.streams[r]},createStream:function(r,e,n){sr.FSStream||(sr.FSStream=function(){},sr.FSStream.prototype={object:{get:function(){return this.node},set:function(r){this.node=r}},isRead:{get:function(){return 1!=(2097155&this.flags)}},isWrite:{get:function(){return 0!=(2097155&this.flags)}},isAppend:{get:function(){return 1024&this.flags}}})
var t=new sr.FSStream
for(var o in r)t[o]=r[o]
r=t
var i=sr.nextfd(e,n)
return r.fd=i,sr.streams[i]=r,r},closeStream:function(r){sr.streams[r]=null},chrdev_stream_ops:{open:function(r){var e=sr.getDevice(r.node.rdev)
r.stream_ops=e.stream_ops,r.stream_ops.open&&r.stream_ops.open(r)},llseek:function(){throw new sr.ErrnoError(70)}},major:function(r){return r>>8},minor:function(r){return 255&r},makedev:function(r,e){return r<<8|e},registerDevice:function(r,e){sr.devices[r]={stream_ops:e}},getDevice:function(r){return sr.devices[r]},getMounts:function(r){for(var e=[],n=[r];n.length;){var t=n.pop()
e.push(t),n.push.apply(n,t.mounts)}return e},syncfs:function(r,e){"function"==typeof r&&(e=r,r=!1),sr.syncFSRequests++,sr.syncFSRequests>1&&f("warning: "+sr.syncFSRequests+" FS.syncfs operations in flight at once, probably just doing extra work")
var n=sr.getMounts(sr.root.mount),t=0
function o(r){return sr.syncFSRequests--,e(r)}function i(r){if(r)return i.errored?void 0:(i.errored=!0,o(r));++t>=n.length&&o(null)}n.forEach((function(e){if(!e.type.syncfs)return i(null)
e.type.syncfs(e,r,i)}))},mount:function(r,e,n){var t,o="/"===n,i=!n
if(o&&sr.root)throw new sr.ErrnoError(10)
if(!o&&!i){var a=sr.lookupPath(n,{follow_mount:!1})
if(n=a.path,t=a.node,sr.isMountpoint(t))throw new sr.ErrnoError(10)
if(!sr.isDir(t.mode))throw new sr.ErrnoError(54)}var s={type:r,opts:e,mountpoint:n,mounts:[]},u=r.mount(s)
return u.mount=s,s.root=u,o?sr.root=u:t&&(t.mounted=s,t.mount&&t.mount.mounts.push(s)),u},unmount:function(r){var e=sr.lookupPath(r,{follow_mount:!1})
if(!sr.isMountpoint(e.node))throw new sr.ErrnoError(28)
var n=e.node,t=n.mounted,o=sr.getMounts(t)
Object.keys(sr.nameTable).forEach((function(r){for(var e=sr.nameTable[r];e;){var n=e.name_next;-1!==o.indexOf(e.mount)&&sr.destroyNode(e),e=n}})),n.mounted=null
var i=n.mount.mounts.indexOf(t)
n.mount.mounts.splice(i,1)},lookup:function(r,e){return r.node_ops.lookup(r,e)},mknod:function(r,e,n){var t=sr.lookupPath(r,{parent:!0}).node,o=tr.basename(r)
if(!o||"."===o||".."===o)throw new sr.ErrnoError(28)
var i=sr.mayCreate(t,o)
if(i)throw new sr.ErrnoError(i)
if(!t.node_ops.mknod)throw new sr.ErrnoError(63)
return t.node_ops.mknod(t,o,e,n)},create:function(r,e){return e=void 0!==e?e:438,e&=4095,e|=32768,sr.mknod(r,e,0)},mkdir:function(r,e){return e=void 0!==e?e:511,e&=1023,e|=16384,sr.mknod(r,e,0)},mkdirTree:function(r,e){for(var n=r.split("/"),t="",o=0;o<n.length;++o)if(n[o]){t+="/"+n[o]
try{sr.mkdir(t,e)}catch(r){if(20!=r.errno)throw r}}},mkdev:function(r,e,n){return void 0===n&&(n=e,e=438),e|=8192,sr.mknod(r,e,n)},symlink:function(r,e){if(!or.resolve(r))throw new sr.ErrnoError(44)
var n=sr.lookupPath(e,{parent:!0}).node
if(!n)throw new sr.ErrnoError(44)
var t=tr.basename(e),o=sr.mayCreate(n,t)
if(o)throw new sr.ErrnoError(o)
if(!n.node_ops.symlink)throw new sr.ErrnoError(63)
return n.node_ops.symlink(n,t,r)},rename:function(r,e){var n,t,o=tr.dirname(r),i=tr.dirname(e),a=tr.basename(r),s=tr.basename(e)
try{n=sr.lookupPath(r,{parent:!0}).node,t=sr.lookupPath(e,{parent:!0}).node}catch(r){throw new sr.ErrnoError(10)}if(!n||!t)throw new sr.ErrnoError(44)
if(n.mount!==t.mount)throw new sr.ErrnoError(75)
var u,c=sr.lookupNode(n,a),l=or.relative(r,i)
if("."!==l.charAt(0))throw new sr.ErrnoError(28)
if("."!==(l=or.relative(e,o)).charAt(0))throw new sr.ErrnoError(55)
try{u=sr.lookupNode(t,s)}catch(r){}if(c!==u){var d=sr.isDir(c.mode),m=sr.mayDelete(n,a,d)
if(m)throw new sr.ErrnoError(m)
if(m=u?sr.mayDelete(t,s,d):sr.mayCreate(t,s))throw new sr.ErrnoError(m)
if(!n.node_ops.rename)throw new sr.ErrnoError(63)
if(sr.isMountpoint(c)||u&&sr.isMountpoint(u))throw new sr.ErrnoError(10)
if(t!==n&&(m=sr.nodePermissions(n,"w")))throw new sr.ErrnoError(m)
try{sr.trackingDelegate.willMovePath&&sr.trackingDelegate.willMovePath(r,e)}catch(n){f("FS.trackingDelegate['willMovePath']('"+r+"', '"+e+"') threw an exception: "+n.message)}sr.hashRemoveNode(c)
try{n.node_ops.rename(c,t,s)}catch(r){throw r}finally{sr.hashAddNode(c)}try{sr.trackingDelegate.onMovePath&&sr.trackingDelegate.onMovePath(r,e)}catch(n){f("FS.trackingDelegate['onMovePath']('"+r+"', '"+e+"') threw an exception: "+n.message)}}},rmdir:function(r){var e=sr.lookupPath(r,{parent:!0}).node,n=tr.basename(r),t=sr.lookupNode(e,n),o=sr.mayDelete(e,n,!0)
if(o)throw new sr.ErrnoError(o)
if(!e.node_ops.rmdir)throw new sr.ErrnoError(63)
if(sr.isMountpoint(t))throw new sr.ErrnoError(10)
try{sr.trackingDelegate.willDeletePath&&sr.trackingDelegate.willDeletePath(r)}catch(e){f("FS.trackingDelegate['willDeletePath']('"+r+"') threw an exception: "+e.message)}e.node_ops.rmdir(e,n),sr.destroyNode(t)
try{sr.trackingDelegate.onDeletePath&&sr.trackingDelegate.onDeletePath(r)}catch(e){f("FS.trackingDelegate['onDeletePath']('"+r+"') threw an exception: "+e.message)}},readdir:function(r){var e=sr.lookupPath(r,{follow:!0}).node
if(!e.node_ops.readdir)throw new sr.ErrnoError(54)
return e.node_ops.readdir(e)},unlink:function(r){var e=sr.lookupPath(r,{parent:!0}).node,n=tr.basename(r),t=sr.lookupNode(e,n),o=sr.mayDelete(e,n,!1)
if(o)throw new sr.ErrnoError(o)
if(!e.node_ops.unlink)throw new sr.ErrnoError(63)
if(sr.isMountpoint(t))throw new sr.ErrnoError(10)
try{sr.trackingDelegate.willDeletePath&&sr.trackingDelegate.willDeletePath(r)}catch(e){f("FS.trackingDelegate['willDeletePath']('"+r+"') threw an exception: "+e.message)}e.node_ops.unlink(e,n),sr.destroyNode(t)
try{sr.trackingDelegate.onDeletePath&&sr.trackingDelegate.onDeletePath(r)}catch(e){f("FS.trackingDelegate['onDeletePath']('"+r+"') threw an exception: "+e.message)}},readlink:function(r){var e=sr.lookupPath(r).node
if(!e)throw new sr.ErrnoError(44)
if(!e.node_ops.readlink)throw new sr.ErrnoError(28)
return or.resolve(sr.getPath(e.parent),e.node_ops.readlink(e))},stat:function(r,e){var n=sr.lookupPath(r,{follow:!e}).node
if(!n)throw new sr.ErrnoError(44)
if(!n.node_ops.getattr)throw new sr.ErrnoError(63)
return n.node_ops.getattr(n)},lstat:function(r){return sr.stat(r,!0)},chmod:function(r,e,n){var t
"string"==typeof r?t=sr.lookupPath(r,{follow:!n}).node:t=r
if(!t.node_ops.setattr)throw new sr.ErrnoError(63)
t.node_ops.setattr(t,{mode:4095&e|-4096&t.mode,timestamp:Date.now()})},lchmod:function(r,e){sr.chmod(r,e,!0)},fchmod:function(r,e){var n=sr.getStream(r)
if(!n)throw new sr.ErrnoError(8)
sr.chmod(n.node,e)},chown:function(r,e,n,t){var o
"string"==typeof r?o=sr.lookupPath(r,{follow:!t}).node:o=r
if(!o.node_ops.setattr)throw new sr.ErrnoError(63)
o.node_ops.setattr(o,{timestamp:Date.now()})},lchown:function(r,e,n){sr.chown(r,e,n,!0)},fchown:function(r,e,n){var t=sr.getStream(r)
if(!t)throw new sr.ErrnoError(8)
sr.chown(t.node,e,n)},truncate:function(r,e){if(e<0)throw new sr.ErrnoError(28)
var n
"string"==typeof r?n=sr.lookupPath(r,{follow:!0}).node:n=r
if(!n.node_ops.setattr)throw new sr.ErrnoError(63)
if(sr.isDir(n.mode))throw new sr.ErrnoError(31)
if(!sr.isFile(n.mode))throw new sr.ErrnoError(28)
var t=sr.nodePermissions(n,"w")
if(t)throw new sr.ErrnoError(t)
n.node_ops.setattr(n,{size:e,timestamp:Date.now()})},ftruncate:function(r,e){var n=sr.getStream(r)
if(!n)throw new sr.ErrnoError(8)
if(0==(2097155&n.flags))throw new sr.ErrnoError(28)
sr.truncate(n.node,e)},utime:function(r,e,n){var t=sr.lookupPath(r,{follow:!0}).node
t.node_ops.setattr(t,{timestamp:Math.max(e,n)})},open:function(e,n,t,o,i){if(""===e)throw new sr.ErrnoError(44)
var a
if(t=void 0===t?438:t,t=64&(n="string"==typeof n?sr.modeStringToFlags(n):n)?4095&t|32768:0,"object"==typeof e)a=e
else{e=tr.normalize(e)
try{a=sr.lookupPath(e,{follow:!(131072&n)}).node}catch(r){}}var s=!1
if(64&n)if(a){if(128&n)throw new sr.ErrnoError(20)}else a=sr.mknod(e,t,0),s=!0
if(!a)throw new sr.ErrnoError(44)
if(sr.isChrdev(a.mode)&&(n&=-513),65536&n&&!sr.isDir(a.mode))throw new sr.ErrnoError(54)
if(!s){var u=sr.mayOpen(a,n)
if(u)throw new sr.ErrnoError(u)}512&n&&sr.truncate(a,0),n&=-131713
var c=sr.createStream({node:a,path:sr.getPath(a),flags:n,seekable:!0,position:0,stream_ops:a.stream_ops,ungotten:[],error:!1},o,i)
c.stream_ops.open&&c.stream_ops.open(c),!r.logReadFiles||1&n||(sr.readFiles||(sr.readFiles={}),e in sr.readFiles||(sr.readFiles[e]=1,f("FS.trackingDelegate error on read file: "+e)))
try{if(sr.trackingDelegate.onOpenFile){var l=0
1!=(2097155&n)&&(l|=sr.tracking.openFlags.READ),0!=(2097155&n)&&(l|=sr.tracking.openFlags.WRITE),sr.trackingDelegate.onOpenFile(e,l)}}catch(r){f("FS.trackingDelegate['onOpenFile']('"+e+"', flags) threw an exception: "+r.message)}return c},close:function(r){if(sr.isClosed(r))throw new sr.ErrnoError(8)
r.getdents&&(r.getdents=null)
try{r.stream_ops.close&&r.stream_ops.close(r)}catch(r){throw r}finally{sr.closeStream(r.fd)}r.fd=null},isClosed:function(r){return null===r.fd},llseek:function(r,e,n){if(sr.isClosed(r))throw new sr.ErrnoError(8)
if(!r.seekable||!r.stream_ops.llseek)throw new sr.ErrnoError(70)
if(0!=n&&1!=n&&2!=n)throw new sr.ErrnoError(28)
return r.position=r.stream_ops.llseek(r,e,n),r.ungotten=[],r.position},read:function(r,e,n,t,o){if(t<0||o<0)throw new sr.ErrnoError(28)
if(sr.isClosed(r))throw new sr.ErrnoError(8)
if(1==(2097155&r.flags))throw new sr.ErrnoError(8)
if(sr.isDir(r.node.mode))throw new sr.ErrnoError(31)
if(!r.stream_ops.read)throw new sr.ErrnoError(28)
var i=void 0!==o
if(i){if(!r.seekable)throw new sr.ErrnoError(70)}else o=r.position
var a=r.stream_ops.read(r,e,n,t,o)
return i||(r.position+=a),a},write:function(r,e,n,t,o,i){if(t<0||o<0)throw new sr.ErrnoError(28)
if(sr.isClosed(r))throw new sr.ErrnoError(8)
if(0==(2097155&r.flags))throw new sr.ErrnoError(8)
if(sr.isDir(r.node.mode))throw new sr.ErrnoError(31)
if(!r.stream_ops.write)throw new sr.ErrnoError(28)
r.seekable&&1024&r.flags&&sr.llseek(r,0,2)
var a=void 0!==o
if(a){if(!r.seekable)throw new sr.ErrnoError(70)}else o=r.position
var s=r.stream_ops.write(r,e,n,t,o,i)
a||(r.position+=s)
try{r.path&&sr.trackingDelegate.onWriteToFile&&sr.trackingDelegate.onWriteToFile(r.path)}catch(e){f("FS.trackingDelegate['onWriteToFile']('"+r.path+"') threw an exception: "+e.message)}return s},allocate:function(r,e,n){if(sr.isClosed(r))throw new sr.ErrnoError(8)
if(e<0||n<=0)throw new sr.ErrnoError(28)
if(0==(2097155&r.flags))throw new sr.ErrnoError(8)
if(!sr.isFile(r.node.mode)&&!sr.isDir(r.node.mode))throw new sr.ErrnoError(43)
if(!r.stream_ops.allocate)throw new sr.ErrnoError(138)
r.stream_ops.allocate(r,e,n)},mmap:function(r,e,n,t,o,i){if(0!=(2&o)&&0==(2&i)&&2!=(2097155&r.flags))throw new sr.ErrnoError(2)
if(1==(2097155&r.flags))throw new sr.ErrnoError(2)
if(!r.stream_ops.mmap)throw new sr.ErrnoError(43)
return r.stream_ops.mmap(r,e,n,t,o,i)},msync:function(r,e,n,t,o){return r&&r.stream_ops.msync?r.stream_ops.msync(r,e,n,t,o):0},munmap:function(r){return 0},ioctl:function(r,e,n){if(!r.stream_ops.ioctl)throw new sr.ErrnoError(59)
return r.stream_ops.ioctl(r,e,n)},readFile:function(r,e){if((e=e||{}).flags=e.flags||"r",e.encoding=e.encoding||"binary","utf8"!==e.encoding&&"binary"!==e.encoding)throw new Error('Invalid encoding type "'+e.encoding+'"')
var n,t=sr.open(r,e.flags),o=sr.stat(r).size,i=new Uint8Array(o)
return sr.read(t,i,0,o,0),"utf8"===e.encoding?n=E(i,0):"binary"===e.encoding&&(n=i),sr.close(t),n},writeFile:function(r,e,n){(n=n||{}).flags=n.flags||"w"
var t=sr.open(r,n.flags,n.mode)
if("string"==typeof e){var o=new Uint8Array(D(e)+1),i=k(e,o,0,o.length)
sr.write(t,o,0,i,void 0,n.canOwn)}else{if(!ArrayBuffer.isView(e))throw new Error("Unsupported data type")
sr.write(t,e,0,e.byteLength,void 0,n.canOwn)}sr.close(t)},cwd:function(){return sr.currentPath},chdir:function(r){var e=sr.lookupPath(r,{follow:!0})
if(null===e.node)throw new sr.ErrnoError(44)
if(!sr.isDir(e.node.mode))throw new sr.ErrnoError(54)
var n=sr.nodePermissions(e.node,"x")
if(n)throw new sr.ErrnoError(n)
sr.currentPath=e.path},createDefaultDirectories:function(){sr.mkdir("/tmp"),sr.mkdir("/home"),sr.mkdir("/home/web_user")},createDefaultDevices:function(){var r
if(sr.mkdir("/dev"),sr.registerDevice(sr.makedev(1,3),{read:function(){return 0},write:function(r,e,n,t,o){return t}}),sr.mkdev("/dev/null",sr.makedev(1,3)),ir.register(sr.makedev(5,0),ir.default_tty_ops),ir.register(sr.makedev(6,0),ir.default_tty1_ops),sr.mkdev("/dev/tty",sr.makedev(5,0)),sr.mkdev("/dev/tty1",sr.makedev(6,0)),"object"==typeof crypto&&"function"==typeof crypto.getRandomValues){var e=new Uint8Array(1)
r=function(){return crypto.getRandomValues(e),e[0]}}r||(r=function(){X("random_device")}),sr.createDevice("/dev","random",r),sr.createDevice("/dev","urandom",r),sr.mkdir("/dev/shm"),sr.mkdir("/dev/shm/tmp")},createSpecialDirectories:function(){sr.mkdir("/proc"),sr.mkdir("/proc/self"),sr.mkdir("/proc/self/fd"),sr.mount({mount:function(){var r=sr.createNode("/proc/self","fd",16895,73)
return r.node_ops={lookup:function(r,e){var n=+e,t=sr.getStream(n)
if(!t)throw new sr.ErrnoError(8)
var o={parent:null,mount:{mountpoint:"fake"},node_ops:{readlink:function(){return t.path}}}
return o.parent=o,o}},r}},{},"/proc/self/fd")},createStandardStreams:function(){r.stdin?sr.createDevice("/dev","stdin",r.stdin):sr.symlink("/dev/tty","/dev/stdin"),r.stdout?sr.createDevice("/dev","stdout",null,r.stdout):sr.symlink("/dev/tty","/dev/stdout"),r.stderr?sr.createDevice("/dev","stderr",null,r.stderr):sr.symlink("/dev/tty1","/dev/stderr")
sr.open("/dev/stdin","r"),sr.open("/dev/stdout","w"),sr.open("/dev/stderr","w")},ensureErrnoError:function(){sr.ErrnoError||(sr.ErrnoError=function(r,e){this.node=e,this.setErrno=function(r){this.errno=r},this.setErrno(r),this.message="FS error"},sr.ErrnoError.prototype=new Error,sr.ErrnoError.prototype.constructor=sr.ErrnoError,[44].forEach((function(r){sr.genericErrors[r]=new sr.ErrnoError(r),sr.genericErrors[r].stack="<generic error, no stack>"})))},staticInit:function(){sr.ensureErrnoError(),sr.nameTable=new Array(4096),sr.mount(ar,{},"/"),sr.createDefaultDirectories(),sr.createDefaultDevices(),sr.createSpecialDirectories(),sr.filesystems={MEMFS:ar}},init:function(e,n,t){sr.init.initialized=!0,sr.ensureErrnoError(),r.stdin=e||r.stdin,r.stdout=n||r.stdout,r.stderr=t||r.stderr,sr.createStandardStreams()},quit:function(){sr.init.initialized=!1
var e=r._fflush
e&&e(0)
for(var n=0;n<sr.streams.length;n++){var t=sr.streams[n]
t&&sr.close(t)}},getMode:function(r,e){var n=0
return r&&(n|=365),e&&(n|=146),n},joinPath:function(r,e){var n=tr.join.apply(null,r)
return e&&"/"==n[0]&&(n=n.substr(1)),n},absolutePath:function(r,e){return or.resolve(e,r)},standardizePath:function(r){return tr.normalize(r)},findObject:function(r,e){var n=sr.analyzePath(r,e)
return n.exists?n.object:(nr(n.error),null)},analyzePath:function(r,e){try{r=(t=sr.lookupPath(r,{follow:!e})).path}catch(r){}var n={isRoot:!1,exists:!1,error:0,name:null,path:null,object:null,parentExists:!1,parentPath:null,parentObject:null}
try{var t=sr.lookupPath(r,{parent:!0})
n.parentExists=!0,n.parentPath=t.path,n.parentObject=t.node,n.name=tr.basename(r),t=sr.lookupPath(r,{follow:!e}),n.exists=!0,n.path=t.path,n.object=t.node,n.name=t.node.name,n.isRoot="/"===t.path}catch(r){n.error=r.errno}return n},createFolder:function(r,e,n,t){var o=tr.join2("string"==typeof r?r:sr.getPath(r),e),i=sr.getMode(n,t)
return sr.mkdir(o,i)},createPath:function(r,e,n,t){r="string"==typeof r?r:sr.getPath(r)
for(var o=e.split("/").reverse();o.length;){var i=o.pop()
if(i){var a=tr.join2(r,i)
try{sr.mkdir(a)}catch(r){}r=a}}return a},createFile:function(r,e,n,t,o){var i=tr.join2("string"==typeof r?r:sr.getPath(r),e),a=sr.getMode(t,o)
return sr.create(i,a)},createDataFile:function(r,e,n,t,o,i){var a=e?tr.join2("string"==typeof r?r:sr.getPath(r),e):r,s=sr.getMode(t,o),u=sr.create(a,s)
if(n){if("string"==typeof n){for(var c=new Array(n.length),l=0,f=n.length;l<f;++l)c[l]=n.charCodeAt(l)
n=c}sr.chmod(u,146|s)
var d=sr.open(u,"w")
sr.write(d,n,0,n.length,0,i),sr.close(d),sr.chmod(u,s)}return u},createDevice:function(r,e,n,t){var o=tr.join2("string"==typeof r?r:sr.getPath(r),e),i=sr.getMode(!!n,!!t)
sr.createDevice.major||(sr.createDevice.major=64)
var a=sr.makedev(sr.createDevice.major++,0)
return sr.registerDevice(a,{open:function(r){r.seekable=!1},close:function(r){t&&t.buffer&&t.buffer.length&&t(10)},read:function(r,e,t,o,i){for(var a=0,s=0;s<o;s++){var u
try{u=n()}catch(r){throw new sr.ErrnoError(29)}if(void 0===u&&0===a)throw new sr.ErrnoError(6)
if(null==u)break
a++,e[t+s]=u}return a&&(r.node.timestamp=Date.now()),a},write:function(r,e,n,o,i){for(var a=0;a<o;a++)try{t(e[n+a])}catch(r){throw new sr.ErrnoError(29)}return o&&(r.node.timestamp=Date.now()),a}}),sr.mkdev(o,i,a)},createLink:function(r,e,n,t,o){var i=tr.join2("string"==typeof r?r:sr.getPath(r),e)
return sr.symlink(n,i)},forceLoadFile:function(r){if(r.isDevice||r.isFolder||r.link||r.contents)return!0
var e=!0
if("undefined"!=typeof XMLHttpRequest)throw new Error("Lazy loading should have been performed (contents set) in createLazyFile, but it was not. Lazy loading only works in web workers. Use --embed-file or --preload-file in emcc on the main thread.")
if(!s)throw new Error("Cannot load without read() or XMLHttpRequest.")
try{r.contents=pr(s(r.url),!0),r.usedBytes=r.contents.length}catch(r){e=!1}return e||nr(29),e},createLazyFile:function(r,e,n,t,i){function a(){this.lengthKnown=!1,this.chunks=[]}if(a.prototype.get=function(r){if(!(r>this.length-1||r<0)){var e=r%this.chunkSize,n=r/this.chunkSize|0
return this.getter(n)[e]}},a.prototype.setDataGetter=function(r){this.getter=r},a.prototype.cacheLength=function(){var r=new XMLHttpRequest
if(r.open("HEAD",n,!1),r.send(null),!(r.status>=200&&r.status<300||304===r.status))throw new Error("Couldn't load "+n+". Status: "+r.status)
var e,t=Number(r.getResponseHeader("Content-length")),o=(e=r.getResponseHeader("Accept-Ranges"))&&"bytes"===e,i=(e=r.getResponseHeader("Content-Encoding"))&&"gzip"===e,a=1048576
o||(a=t)
var s=this
s.setDataGetter((function(r){var e=r*a,o=(r+1)*a-1
if(o=Math.min(o,t-1),void 0===s.chunks[r]&&(s.chunks[r]=function(r,e){if(r>e)throw new Error("invalid range ("+r+", "+e+") or no bytes requested!")
if(e>t-1)throw new Error("only "+t+" bytes available! programmer error!")
var o=new XMLHttpRequest
if(o.open("GET",n,!1),t!==a&&o.setRequestHeader("Range","bytes="+r+"-"+e),"undefined"!=typeof Uint8Array&&(o.responseType="arraybuffer"),o.overrideMimeType&&o.overrideMimeType("text/plain; charset=x-user-defined"),o.send(null),!(o.status>=200&&o.status<300||304===o.status))throw new Error("Couldn't load "+n+". Status: "+o.status)
return void 0!==o.response?new Uint8Array(o.response||[]):pr(o.responseText||"",!0)}(e,o)),void 0===s.chunks[r])throw new Error("doXHR failed!")
return s.chunks[r]})),!i&&t||(a=t=1,t=this.getter(0).length,a=t,l("LazyFiles on gzip forces download of the whole file when length is accessed")),this._length=t,this._chunkSize=a,this.lengthKnown=!0},"undefined"!=typeof XMLHttpRequest){if(!o)throw"Cannot do synchronous binary XHRs outside webworkers in modern browsers. Use --embed-file or --preload-file in emcc"
var s=new a
Object.defineProperties(s,{length:{get:function(){return this.lengthKnown||this.cacheLength(),this._length}},chunkSize:{get:function(){return this.lengthKnown||this.cacheLength(),this._chunkSize}}})
var u={isDevice:!1,contents:s}}else u={isDevice:!1,url:n}
var c=sr.createFile(r,e,u,t,i)
u.contents?c.contents=u.contents:u.url&&(c.contents=null,c.url=u.url),Object.defineProperties(c,{usedBytes:{get:function(){return this.contents.length}}})
var f={}
return Object.keys(c.stream_ops).forEach((function(r){var e=c.stream_ops[r]
f[r]=function(){if(!sr.forceLoadFile(c))throw new sr.ErrnoError(29)
return e.apply(null,arguments)}})),f.read=function(r,e,n,t,o){if(!sr.forceLoadFile(c))throw new sr.ErrnoError(29)
var i=r.node.contents
if(o>=i.length)return 0
var a=Math.min(i.length-o,t)
if(i.slice)for(var s=0;s<a;s++)e[n+s]=i[o+s]
else for(s=0;s<a;s++)e[n+s]=i.get(o+s)
return a},c.stream_ops=f,c},createPreloadedFile:function(e,n,t,o,i,a,s,u,c,l){Browser.init()
var f=n?or.resolve(tr.join2(e,n)):e
function d(t){function d(r){l&&l(),u||sr.createDataFile(e,n,r,o,i,c),a&&a(),V()}var m=!1
r.preloadPlugins.forEach((function(r){m||r.canHandle(f)&&(r.handle(t,f,d,(function(){s&&s(),V()})),m=!0)})),m||d(t)}q(),"string"==typeof t?Browser.asyncLoad(t,(function(r){d(r)}),s):d(t)},indexedDB:function(){return window.indexedDB||window.mozIndexedDB||window.webkitIndexedDB||window.msIndexedDB},DB_NAME:function(){return"EM_FS_"+window.location.pathname},DB_VERSION:20,DB_STORE_NAME:"FILE_DATA",saveFilesToDB:function(r,e,n){e=e||function(){},n=n||function(){}
var t=sr.indexedDB()
try{var o=t.open(sr.DB_NAME(),sr.DB_VERSION)}catch(r){return n(r)}o.onupgradeneeded=function(){l("creating db"),o.result.createObjectStore(sr.DB_STORE_NAME)},o.onsuccess=function(){var t=o.result.transaction([sr.DB_STORE_NAME],"readwrite"),i=t.objectStore(sr.DB_STORE_NAME),a=0,s=0,u=r.length
function c(){0==s?e():n()}r.forEach((function(r){var e=i.put(sr.analyzePath(r).object.contents,r)
e.onsuccess=function(){++a+s==u&&c()},e.onerror=function(){s++,a+s==u&&c()}})),t.onerror=n},o.onerror=n},loadFilesFromDB:function(r,e,n){e=e||function(){},n=n||function(){}
var t=sr.indexedDB()
try{var o=t.open(sr.DB_NAME(),sr.DB_VERSION)}catch(r){return n(r)}o.onupgradeneeded=n,o.onsuccess=function(){var t=o.result
try{var i=t.transaction([sr.DB_STORE_NAME],"readonly")}catch(r){return void n(r)}var a=i.objectStore(sr.DB_STORE_NAME),s=0,u=0,c=r.length
function l(){0==u?e():n()}r.forEach((function(r){var e=a.get(r)
e.onsuccess=function(){sr.analyzePath(r).exists&&sr.unlink(r),sr.createDataFile(tr.dirname(r),tr.basename(r),e.result,!0,!0,!0),++s+u==c&&l()},e.onerror=function(){u++,s+u==c&&l()}})),i.onerror=n},o.onerror=n}},ur={mappings:{},DEFAULT_POLLMASK:5,umask:511,calculateAt:function(r,e){if("/"!==e[0]){var n
if(-100===r)n=sr.cwd()
else{var t=sr.getStream(r)
if(!t)throw new sr.ErrnoError(8)
n=t.path}e=tr.join2(n,e)}return e},doStat:function(r,e,n){try{var t=r(e)}catch(r){if(r&&r.node&&tr.normalize(e)!==tr.normalize(sr.getPath(r.node)))return-54
throw r}return x[n>>2]=t.dev,x[n+4>>2]=0,x[n+8>>2]=t.ino,x[n+12>>2]=t.mode,x[n+16>>2]=t.nlink,x[n+20>>2]=t.uid,x[n+24>>2]=t.gid,x[n+28>>2]=t.rdev,x[n+32>>2]=0,Z=[t.size>>>0,(Y=t.size,+O(Y)>=1?Y>0?(0|U(+I(Y/4294967296),4294967295))>>>0:~~+L((Y-+(~~Y>>>0))/4294967296)>>>0:0)],x[n+40>>2]=Z[0],x[n+44>>2]=Z[1],x[n+48>>2]=4096,x[n+52>>2]=t.blocks,x[n+56>>2]=t.atime.getTime()/1e3|0,x[n+60>>2]=0,x[n+64>>2]=t.mtime.getTime()/1e3|0,x[n+68>>2]=0,x[n+72>>2]=t.ctime.getTime()/1e3|0,x[n+76>>2]=0,Z=[t.ino>>>0,(Y=t.ino,+O(Y)>=1?Y>0?(0|U(+I(Y/4294967296),4294967295))>>>0:~~+L((Y-+(~~Y>>>0))/4294967296)>>>0:0)],x[n+80>>2]=Z[0],x[n+84>>2]=Z[1],0},doMsync:function(r,e,n,t,o){var i=P.slice(r,r+n)
sr.msync(e,i,o,n,t)},doMkdir:function(r,e){return"/"===(r=tr.normalize(r))[r.length-1]&&(r=r.substr(0,r.length-1)),sr.mkdir(r,e,0),0},doMknod:function(r,e,n){switch(61440&e){case 32768:case 8192:case 24576:case 4096:case 49152:break
default:return-28}return sr.mknod(r,e,n),0},doReadlink:function(r,e,n){if(n<=0)return-28
var t=sr.readlink(r),o=Math.min(n,D(t)),i=F[e+o]
return b(t,e,n+1),F[e+o]=i,o},doAccess:function(r,e){if(-8&e)return-28
var n
if(!(n=sr.lookupPath(r,{follow:!0}).node))return-44
var t=""
return 4&e&&(t+="r"),2&e&&(t+="w"),1&e&&(t+="x"),t&&sr.nodePermissions(n,t)?-2:0},doDup:function(r,e,n){var t=sr.getStream(n)
return t&&sr.close(t),sr.open(r,e,0,n,n).fd},doReadv:function(r,e,n,t){for(var o=0,i=0;i<n;i++){var a=x[e+8*i>>2],s=x[e+(8*i+4)>>2],u=sr.read(r,F,a,s,t)
if(u<0)return-1
if(o+=u,u<s)break}return o},doWritev:function(r,e,n,t){for(var o=0,i=0;i<n;i++){var a=x[e+8*i>>2],s=x[e+(8*i+4)>>2],u=sr.write(r,F,a,s,t)
if(u<0)return-1
o+=u}return o},varargs:void 0,get:function(){return ur.varargs+=4,x[ur.varargs-4>>2]},getStr:function(r){return _(r)},getStreamFromFD:function(r){var e=sr.getStream(r)
if(!e)throw new sr.ErrnoError(8)
return e},get64:function(r,e){return r}}
function cr(r){try{return p.grow(r-S.byteLength+65535>>>16),R(p.buffer),1}catch(r){}}var lr={}
function fr(){if(!fr.strings){var r={USER:"web_user",LOGNAME:"web_user",PATH:"/",PWD:"/",HOME:"/home/web_user",LANG:("object"==typeof navigator&&navigator.languages&&navigator.languages[0]||"C").replace("-","_")+".UTF-8",_:i||"./this.program"}
for(var e in lr)r[e]=lr[e]
var n=[]
for(var e in r)n.push(e+"="+r[e])
fr.strings=n}return fr.strings}var dr=[]
var mr=function(r,e,n,t){r||(r=this),this.parent=r,this.mount=r.mount,this.mounted=null,this.id=sr.nextInode++,this.name=e,this.mode=n,this.node_ops={},this.stream_ops={},this.rdev=t}
function pr(r,e,n){var t=n>0?n:D(r)+1,o=new Array(t),i=k(r,o,0,o.length)
return e&&(o.length=i),o}Object.defineProperties(mr.prototype,{read:{get:function(){return 365==(365&this.mode)},set:function(r){r?this.mode|=365:this.mode&=-366}},write:{get:function(){return 146==(146&this.mode)},set:function(r){r?this.mode|=146:this.mode&=-147}},isFolder:{get:function(){return sr.isDir(this.mode)}},isDevice:{get:function(){return sr.isChrdev(this.mode)}}}),sr.FSNode=mr,sr.staticInit()
var hr,vr={a:function(r,e,n,t){X("Assertion failed: "+_(r)+", at: "+[e?_(e):"unknown filename",n,t?_(t):"unknown function"])},D:function(r,e){return function(r,e){var n
if(0===r)n=Date.now()
else{if(1!==r&&4!==r)return nr(28),-1
n=Q()}return x[e>>2]=n/1e3|0,x[e+4>>2]=n%1e3*1e3*1e3|0,0}(r,e)},B:function(r,e){return nr(63),-1},J:function(r,e){try{return r=ur.getStr(r),ur.doAccess(r,e)}catch(r){return void 0!==sr&&r instanceof sr.ErrnoError||X(r),-r.errno}},r:function(r,e,n){ur.varargs=n
try{var t=ur.getStreamFromFD(r)
switch(e){case 0:return(o=ur.get())<0?-28:sr.open(t.path,t.flags,0,o).fd
case 1:case 2:return 0
case 3:return t.flags
case 4:var o=ur.get()
return t.flags|=o,0
case 12:o=ur.get()
return A[o+0>>1]=2,0
case 13:case 14:return 0
case 16:case 8:return-28
case 9:return nr(28),-1
default:return-28}}catch(r){return void 0!==sr&&r instanceof sr.ErrnoError||X(r),-r.errno}},L:function(r,e){try{var n=ur.getStreamFromFD(r)
return ur.doStat(sr.stat,n.path,e)}catch(r){return void 0!==sr&&r instanceof sr.ErrnoError||X(r),-r.errno}},I:function(){return 42},F:function(r,e,n){ur.varargs=n
try{var t=ur.getStreamFromFD(r)
switch(e){case 21509:case 21505:return t.tty?0:-59
case 21510:case 21511:case 21512:case 21506:case 21507:case 21508:return t.tty?0:-59
case 21519:if(!t.tty)return-59
var o=ur.get()
return x[o>>2]=0,0
case 21520:return t.tty?-28:-59
case 21531:o=ur.get()
return sr.ioctl(t,e,o)
case 21523:case 21524:return t.tty?0:-59
default:X("bad ioctl syscall "+e)}}catch(r){return void 0!==sr&&r instanceof sr.ErrnoError||X(r),-r.errno}},G:function(r,e,n,t,o,i){try{return function(r,e,n,t,o,i){var a
i<<=12
var s=!1
if(0!=(16&t)&&r%16384!=0)return-28
if(0!=(32&t)){if(!(a=kr(16384,e)))return-48
_r(a,0,e),s=!0}else{var u=sr.getStream(o)
if(!u)return-8
var c=sr.mmap(u,r,e,i,n,t)
a=c.ptr,s=c.allocated}return ur.mappings[a]={malloc:a,len:e,allocated:s,fd:o,prot:n,flags:t,offset:i},a}(r,e,n,t,o,i)}catch(r){return void 0!==sr&&r instanceof sr.ErrnoError||X(r),-r.errno}},H:function(r,e){try{return function(r,e){if(-1==(0|r)||0===e)return-28
var n=ur.mappings[r]
if(!n)return 0
if(e===n.len){var t=sr.getStream(n.fd)
2&n.prot&&ur.doMsync(r,t,e,n.flags,n.offset),sr.munmap(t),ur.mappings[r]=null,n.allocated&&yr(n.malloc)}return 0}(r,e)}catch(r){return void 0!==sr&&r instanceof sr.ErrnoError||X(r),-r.errno}},o:function(r,e,n){ur.varargs=n
try{var t=ur.getStr(r),o=ur.get()
return sr.open(t,e,o).fd}catch(r){return void 0!==sr&&r instanceof sr.ErrnoError||X(r),-r.errno}},K:function(r,e){try{return r=ur.getStr(r),ur.doStat(sr.stat,r,e)}catch(r){return void 0!==sr&&r instanceof sr.ErrnoError||X(r),-r.errno}},C:function(r){try{return r=ur.getStr(r),sr.unlink(r),0}catch(r){return void 0!==sr&&r instanceof sr.ErrnoError||X(r),-r.errno}},N:function(){X()},u:function(r,e,n){var t=function(r,e){var n
dr.length=0,e>>=2
for(;n=P[r++];)dr.push(n<105?M[++e>>1]:x[e]),++e
return dr}(e,n)
return rr[r].apply(null,t)},d:function(r,e){!function(r,e){throw br(r,e||1),"longjmp"}(r,e)},w:function(r,e,n){P.copyWithin(r,e,e+n)},x:function(r){r>>>=0
var e=P.length
if(r>2147483648)return!1
for(var n,t,o=1;o<=4;o*=2){var i=e*(1+.2/o)
if(i=Math.min(i,r+100663296),cr(Math.min(2147483648,((n=Math.max(16777216,r,i))%(t=65536)>0&&(n+=t-n%t),n))))return!0}return!1},z:function(r,e){var n=0
return fr().forEach((function(t,o){var i=e+n
x[r+4*o>>2]=i,function(r,e,n){for(var t=0;t<r.length;++t)F[e++>>0]=r.charCodeAt(t)
n||(F[e>>0]=0)}(t,i),n+=t.length+1})),0},A:function(r,e){var n=fr()
x[r>>2]=n.length
var t=0
return n.forEach((function(r){t+=r.length+1})),x[e>>2]=t,0},j:function(e){!function(e,n){if(n&&m&&0===e)return
m||(w=!0,r.onExit&&r.onExit(e))
a(e,new Lr(e))}(e)},k:function(r){try{var e=ur.getStreamFromFD(r)
return sr.close(e),0}catch(r){return void 0!==sr&&r instanceof sr.ErrnoError||X(r),r.errno}},y:function(r,e){try{var n=ur.getStreamFromFD(r),t=n.tty?2:sr.isDir(n.mode)?3:sr.isLink(n.mode)?7:4
return F[e>>0]=t,0}catch(r){return void 0!==sr&&r instanceof sr.ErrnoError||X(r),r.errno}},E:function(r,e,n,t){try{var o=ur.getStreamFromFD(r),i=ur.doReadv(o,e,n)
return x[t>>2]=i,0}catch(r){return void 0!==sr&&r instanceof sr.ErrnoError||X(r),r.errno}},v:function(r,e,n,t,o){try{var i=ur.getStreamFromFD(r),a=4294967296*n+(e>>>0)
return a<=-9007199254740992||a>=9007199254740992?-61:(sr.llseek(i,a,t),Z=[i.position>>>0,(Y=i.position,+O(Y)>=1?Y>0?(0|U(+I(Y/4294967296),4294967295))>>>0:~~+L((Y-+(~~Y>>>0))/4294967296)>>>0:0)],x[o>>2]=Z[0],x[o+4>>2]=Z[1],i.getdents&&0===a&&0===t&&(i.getdents=null),0)}catch(r){return void 0!==sr&&r instanceof sr.ErrnoError||X(r),r.errno}},n:function(r,e,n,t){try{var o=ur.getStreamFromFD(r),i=ur.doWritev(o,e,n)
return x[t>>2]=i,0}catch(r){return void 0!==sr&&r instanceof sr.ErrnoError||X(r),r.errno}},c:function(){return 0|h},O:function(r){var e=Dr()
try{return Or(r)}catch(r){if(Sr(e),r!==r+0&&"longjmp"!==r)throw r
br(1,0)}},t:function(r){var e=Dr()
try{return Cr(r)}catch(r){if(Sr(e),r!==r+0&&"longjmp"!==r)throw r
br(1,0)}},i:function(r,e){var n=Dr()
try{return Tr(r,e)}catch(r){if(Sr(n),r!==r+0&&"longjmp"!==r)throw r
br(1,0)}},g:function(r,e,n){var t=Dr()
try{return jr(r,e,n)}catch(r){if(Sr(t),r!==r+0&&"longjmp"!==r)throw r
br(1,0)}},e:function(r,e,n,t){var o=Dr()
try{return Br(r,e,n,t)}catch(r){if(Sr(o),r!==r+0&&"longjmp"!==r)throw r
br(1,0)}},M:function(r,e,n,t,o,i,a){var s=Dr()
try{return Nr(r,e,n,t,o,i,a)}catch(r){if(Sr(s),r!==r+0&&"longjmp"!==r)throw r
br(1,0)}},h:function(r){var e=Dr()
try{Pr(r)}catch(r){if(Sr(e),r!==r+0&&"longjmp"!==r)throw r
br(1,0)}},f:function(r,e){var n=Dr()
try{Ar(r,e)}catch(r){if(Sr(n),r!==r+0&&"longjmp"!==r)throw r
br(1,0)}},l:function(r,e,n){var t=Dr()
try{xr(r,e,n)}catch(r){if(Sr(t),r!==r+0&&"longjmp"!==r)throw r
br(1,0)}},q:function(r,e,n,t){var o=Dr()
try{Mr(r,e,n,t)}catch(r){if(Sr(o),r!==r+0&&"longjmp"!==r)throw r
br(1,0)}},p:function(r,e,n,t,o){var i=Dr()
try{Rr(r,e,n,t,o)}catch(r){if(Sr(i),r!==r+0&&"longjmp"!==r)throw r
br(1,0)}},m:function(r,e,n,t,o,i){var a=Dr()
try{zr(r,e,n,t,o,i)}catch(r){if(Sr(a),r!==r+0&&"longjmp"!==r)throw r
br(1,0)}},memory:p,b:function(r){h=0|r},table:v,s:function(r){return 0!==r&&_r(r,0,16),0}},wr=(function(){var e={a:vr}
function n(e,n){var t=e.exports
r.asm=t,V()}function i(r){n(r.instance)}function a(r){return(d||!t&&!o||"function"!=typeof fetch?new Promise((function(r,e){r(J())})):fetch($,{credentials:"same-origin"}).then((function(r){if(!r.ok)throw"failed to load wasm binary file at '"+$+"'"
return r.arrayBuffer()})).catch((function(){return J()}))).then((function(r){return WebAssembly.instantiate(r,e)})).then(r,(function(r){f("failed to asynchronously prepare wasm: "+r),X(r)}))}if(q(),r.instantiateWasm)try{return r.instantiateWasm(e,n)}catch(r){return f("Module.instantiateWasm callback failed with error: "+r),!1}(function(){if(d||"function"!=typeof WebAssembly.instantiateStreaming||G($)||"function"!=typeof fetch)return a(i)
fetch($,{credentials:"same-origin"}).then((function(r){return WebAssembly.instantiateStreaming(r,e).then(i,(function(r){return f("wasm streaming compile failed: "+r),f("falling back to ArrayBuffer instantiation"),a(i)}))}))})()}(),r.___wasm_call_ctors=function(){return(wr=r.___wasm_call_ctors=r.asm.P).apply(null,arguments)}),gr=(r._vizLastErrorMessage=function(){return(r._vizLastErrorMessage=r.asm.Q).apply(null,arguments)},r._vizCreateFile=function(){return(r._vizCreateFile=r.asm.R).apply(null,arguments)},r._vizSetY_invert=function(){return(r._vizSetY_invert=r.asm.S).apply(null,arguments)},r._vizSetNop=function(){return(r._vizSetNop=r.asm.T).apply(null,arguments)},r._vizRenderFromString=function(){return(r._vizRenderFromString=r.asm.U).apply(null,arguments)},r._malloc=function(){return(gr=r._malloc=r.asm.V).apply(null,arguments)}),yr=r._free=function(){return(yr=r._free=r.asm.W).apply(null,arguments)},Er=(r._dtopen=function(){return(r._dtopen=r.asm.X).apply(null,arguments)},r.___errno_location=function(){return(Er=r.___errno_location=r.asm.Y).apply(null,arguments)}),_r=r._memset=function(){return(_r=r._memset=r.asm.Z).apply(null,arguments)},kr=(r._dtextract=function(){return(r._dtextract=r.asm._).apply(null,arguments)},r._dtdisc=function(){return(r._dtdisc=r.asm.$).apply(null,arguments)},r._memalign=function(){return(kr=r._memalign=r.asm.aa).apply(null,arguments)}),br=r._setThrew=function(){return(br=r._setThrew=r.asm.ba).apply(null,arguments)},Dr=r.stackSave=function(){return(Dr=r.stackSave=r.asm.ca).apply(null,arguments)},Sr=r.stackRestore=function(){return(Sr=r.stackRestore=r.asm.da).apply(null,arguments)},Fr=r.stackAlloc=function(){return(Fr=r.stackAlloc=r.asm.ea).apply(null,arguments)},Pr=r.dynCall_v=function(){return(Pr=r.dynCall_v=r.asm.fa).apply(null,arguments)},Ar=r.dynCall_vi=function(){return(Ar=r.dynCall_vi=r.asm.ga).apply(null,arguments)},xr=r.dynCall_vii=function(){return(xr=r.dynCall_vii=r.asm.ha).apply(null,arguments)},Mr=r.dynCall_viii=function(){return(Mr=r.dynCall_viii=r.asm.ia).apply(null,arguments)},Rr=r.dynCall_viiii=function(){return(Rr=r.dynCall_viiii=r.asm.ja).apply(null,arguments)},zr=r.dynCall_viiiii=function(){return(zr=r.dynCall_viiiii=r.asm.ka).apply(null,arguments)},Cr=r.dynCall_i=function(){return(Cr=r.dynCall_i=r.asm.la).apply(null,arguments)},Tr=r.dynCall_ii=function(){return(Tr=r.dynCall_ii=r.asm.ma).apply(null,arguments)},jr=r.dynCall_iii=function(){return(jr=r.dynCall_iii=r.asm.na).apply(null,arguments)},Br=r.dynCall_iiii=function(){return(Br=r.dynCall_iiii=r.asm.oa).apply(null,arguments)},Nr=r.dynCall_iiiiiii=function(){return(Nr=r.dynCall_iiiiiii=r.asm.pa).apply(null,arguments)},Or=r.dynCall_d=function(){return(Or=r.dynCall_d=r.asm.qa).apply(null,arguments)}
function Lr(r){this.name="ExitStatus",this.message="Program terminated with exit("+r+")",this.status=r}function Ir(e){function n(){hr||(hr=!0,r.calledRun=!0,w||(r.noFSInit||sr.init.initialized||sr.init(),ir.init(),C(j),sr.ignorePermissions=!1,C(B),r.onRuntimeInitialized&&r.onRuntimeInitialized(),function(){if(r.postRun)for("function"==typeof r.postRun&&(r.postRun=[r.postRun]);r.postRun.length;)e=r.postRun.shift(),N.unshift(e)
var e
C(N)}()))}H>0||(!function(){if(r.preRun)for("function"==typeof r.preRun&&(r.preRun=[r.preRun]);r.preRun.length;)e=r.preRun.shift(),T.unshift(e)
var e
C(T)}(),H>0||(r.setStatus?(r.setStatus("Running..."),setTimeout((function(){setTimeout((function(){r.setStatus("")}),1),n()}),1)):n()))}if(r.ccall=function(e,n,t,o,i){var a={string:function(r){var e=0
if(null!=r&&0!==r){var n=1+(r.length<<2)
b(r,e=Fr(n),n)}return e},array:function(r){var e=Fr(r.length)
return function(r,e){F.set(r,e)}(r,e),e}},s=function(e){var n=r["_"+e]
return g(n,"Cannot call unknown function "+e+", make sure it is exported"),n}(e),u=[],c=0
if(o)for(var l=0;l<o.length;l++){var f=a[t[l]]
f?(0===c&&(c=Dr()),u[l]=f(o[l])):u[l]=o[l]}var d=s.apply(null,u)
return d=function(r){return"string"===n?_(r):"boolean"===n?Boolean(r):r}(d),0!==c&&Sr(c),d},r.UTF8ToString=_,W=function r(){hr||Ir(),hr||(W=r)},r.run=Ir,r.preInit)for("function"==typeof r.preInit&&(r.preInit=[r.preInit]);r.preInit.length>0;)r.preInit.pop()()
return m=!0,Ir(),r})}("undefined"!=typeof self?self:window)
const e=new class{constructor(r,e){let n=void 0,t=!1,o=new Promise((e,o)=>{try{n=r(),n.onRuntimeInitialized=()=>{t=!0,e()}}catch(r){o(r)}})
this.render=async(i,a)=>{t||await o
try{return e(n,i,a)}catch(e){throw n=r(),e}}}}(Viz.Module,Viz.render),n={format:"svg",engine:"dot",files:[],images:[],yInvert:!1,nop:0}
r.viz=function(r,t){const o=t?Object.assign({},n,t):n
return e.render(r,o)}}()
