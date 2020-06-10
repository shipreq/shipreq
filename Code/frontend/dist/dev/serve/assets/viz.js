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
var d,m,p,h=0,v=function(r){h=r}
r.wasmBinary&&(d=r.wasmBinary),r.noExitRuntime&&(m=r.noExitRuntime),"object"!=typeof WebAssembly&&f("no native wasm support detected")
var w=new WebAssembly.Table({initial:425,maximum:425,element:"anyfunc"}),g=!1
function y(e){var n,t=r["_"+e]
return n="Cannot call unknown function "+e+", make sure it is exported",t||G("Assertion failed: "+n),t}var E="undefined"!=typeof TextDecoder?new TextDecoder("utf8"):void 0
function _(r,e,n){for(var t=e+n,o=e;r[o]&&!(o>=t);)++o
if(o-e>16&&r.subarray&&E)return E.decode(r.subarray(e,o))
for(var i="";e<o;){var a=r[e++]
if(128&a){var s=63&r[e++]
if(192!=(224&a)){var u=63&r[e++]
if((a=224==(240&a)?(15&a)<<12|s<<6|u:(7&a)<<18|s<<12|u<<6|63&r[e++])<65536)i+=String.fromCharCode(a)
else{var c=a-65536
i+=String.fromCharCode(55296|c>>10,56320|1023&c)}}else i+=String.fromCharCode((31&a)<<6|s)}else i+=String.fromCharCode(a)}return i}function k(r,e){return r?_(A,r,e):""}function b(r,e,n,t){if(!(t>0))return 0
for(var o=n,i=n+t-1,a=0;a<r.length;++a){var s=r.charCodeAt(a)
if(s>=55296&&s<=57343)s=65536+((1023&s)<<10)|1023&r.charCodeAt(++a)
if(s<=127){if(n>=i)break
e[n++]=s}else if(s<=2047){if(n+1>=i)break
e[n++]=192|s>>6,e[n++]=128|63&s}else if(s<=65535){if(n+2>=i)break
e[n++]=224|s>>12,e[n++]=128|s>>6&63,e[n++]=128|63&s}else{if(n+3>=i)break
e[n++]=240|s>>18,e[n++]=128|s>>12&63,e[n++]=128|s>>6&63,e[n++]=128|63&s}}return e[n]=0,n-o}function D(r,e,n){return b(r,A,e,n)}function S(r){for(var e=0,n=0;n<r.length;++n){var t=r.charCodeAt(n)
t>=55296&&t<=57343&&(t=65536+((1023&t)<<10)|1023&r.charCodeAt(++n)),t<=127?++e:e+=t<=2047?2:t<=65535?3:4}return e}var F,P,A,x,M,R
var z,C=r.INITIAL_MEMORY||16777216
function T(e){for(;e.length>0;){var n=e.shift()
if("function"!=typeof n){var t=n.func
"number"==typeof t?void 0===n.arg?r.dynCall_v(t):r.dynCall_vi(t,n.arg):t(void 0===n.arg?null:n.arg)}else n(r)}}(p=r.wasmMemory?r.wasmMemory:new WebAssembly.Memory({initial:C/65536,maximum:C/65536}))&&(F=p.buffer),C=F.byteLength,F=z=F,r.HEAP8=P=new Int8Array(z),r.HEAP16=x=new Int16Array(z),r.HEAP32=M=new Int32Array(z),r.HEAPU8=A=new Uint8Array(z),r.HEAPU16=new Uint16Array(z),r.HEAPU32=new Uint32Array(z),r.HEAPF32=new Float32Array(z),r.HEAPF64=R=new Float64Array(z),M[49648]=5441632
var j=[],B=[],N=[],O=[]
var L=Math.abs,I=Math.ceil,U=Math.floor,H=Math.min,W=0,q=null
function V(e){W++,r.monitorRunDependencies&&r.monitorRunDependencies(W)}function X(e){if(W--,r.monitorRunDependencies&&r.monitorRunDependencies(W),0==W&&q){var n=q
q=null,n()}}function G(e){throw r.onAbort&&r.onAbort(e),l(e+=""),f(e),g=!0,e="abort("+e+"). Build with -s ASSERTIONS=1 for more info.",new WebAssembly.RuntimeError(e)}r.preloadedImages={},r.preloadedAudios={}
function K(r){return e=r,n="data:application/octet-stream;base64,",String.prototype.startsWith?e.startsWith(n):0===e.indexOf(n)
var e,n}var Y,Z,$,J=""
function Q(){try{if(d)return new Uint8Array(d)
if(u)return u(J)
throw"both async and sync fetching of the wasm failed"}catch(r){G(r)}}K(J)||(Y=J,J=r.locateFile?r.locateFile(Y,c):c+Y)
var rr,er={1024:function(r,e){var n=Pointer_stringify(r),t=Pointer_stringify(e)
ur.createPath("/",or.dirname(n)),ur.writeFile(or.join("/",n),t)}}
function nr(){var e=function(){var r=new Error
if(!r.stack){try{throw new Error}catch(e){r=e}if(!r.stack)return"(no stack trace available)"}return r.stack.toString()}()
return r.extraStackTrace&&(e+="\n"+r.extraStackTrace()),e.replace(/\b_Z[\w\d_]+/g,(function(r){return r==r?r:r+" ["+r+"]"}))}B.push({func:function(){yr()}}),rr=function(){return performance.now()}
function tr(r){return M[br()>>2]=r,r}var or={splitPath:function(r){return/^(\/?|)([\s\S]*?)((?:\.{1,2}|[^\/]+?|)(\.[^.\/]*|))(?:[\/]*)$/.exec(r).slice(1)},normalizeArray:function(r,e){for(var n=0,t=r.length-1;t>=0;t--){var o=r[t]
"."===o?r.splice(t,1):".."===o?(r.splice(t,1),n++):n&&(r.splice(t,1),n--)}if(e)for(;n;n--)r.unshift("..")
return r},normalize:function(r){var e="/"===r.charAt(0),n="/"===r.substr(-1)
return(r=or.normalizeArray(r.split("/").filter((function(r){return!!r})),!e).join("/"))||e||(r="."),r&&n&&(r+="/"),(e?"/":"")+r},dirname:function(r){var e=or.splitPath(r),n=e[0],t=e[1]
return n||t?(t&&(t=t.substr(0,t.length-1)),n+t):"."},basename:function(r){if("/"===r)return"/"
var e=r.lastIndexOf("/")
return-1===e?r:r.substr(e+1)},extname:function(r){return or.splitPath(r)[3]},join:function(){var r=Array.prototype.slice.call(arguments,0)
return or.normalize(r.join("/"))},join2:function(r,e){return or.normalize(r+"/"+e)}},ir={resolve:function(){for(var r="",e=!1,n=arguments.length-1;n>=-1&&!e;n--){var t=n>=0?arguments[n]:ur.cwd()
if("string"!=typeof t)throw new TypeError("Arguments to path.resolve must be strings")
if(!t)return""
r=t+"/"+r,e="/"===t.charAt(0)}return(e?"/":"")+(r=or.normalizeArray(r.split("/").filter((function(r){return!!r})),!e).join("/"))||"."},relative:function(r,e){function n(r){for(var e=0;e<r.length&&""===r[e];e++);for(var n=r.length-1;n>=0&&""===r[n];n--);return e>n?[]:r.slice(e,n-e+1)}r=ir.resolve(r).substr(1),e=ir.resolve(e).substr(1)
for(var t=n(r.split("/")),o=n(e.split("/")),i=Math.min(t.length,o.length),a=i,s=0;s<i;s++)if(t[s]!==o[s]){a=s
break}var u=[]
for(s=a;s<t.length;s++)u.push("..")
return(u=u.concat(o.slice(a))).join("/")}},ar={ttys:[],init:function(){},shutdown:function(){},register:function(r,e){ar.ttys[r]={input:[],output:[],ops:e},ur.registerDevice(r,ar.stream_ops)},stream_ops:{open:function(r){var e=ar.ttys[r.node.rdev]
if(!e)throw new ur.ErrnoError(43)
r.tty=e,r.seekable=!1},close:function(r){r.tty.ops.flush(r.tty)},flush:function(r){r.tty.ops.flush(r.tty)},read:function(r,e,n,t,o){if(!r.tty||!r.tty.ops.get_char)throw new ur.ErrnoError(60)
for(var i=0,a=0;a<t;a++){var s
try{s=r.tty.ops.get_char(r.tty)}catch(r){throw new ur.ErrnoError(29)}if(void 0===s&&0===i)throw new ur.ErrnoError(6)
if(null==s)break
i++,e[n+a]=s}return i&&(r.node.timestamp=Date.now()),i},write:function(r,e,n,t,o){if(!r.tty||!r.tty.ops.put_char)throw new ur.ErrnoError(60)
try{for(var i=0;i<t;i++)r.tty.ops.put_char(r.tty,e[n+i])}catch(r){throw new ur.ErrnoError(29)}return t&&(r.node.timestamp=Date.now()),i}},default_tty_ops:{get_char:function(r){if(!r.input.length){var e=null
if("undefined"!=typeof window&&"function"==typeof window.prompt?null!==(e=window.prompt("Input: "))&&(e+="\n"):"function"==typeof readline&&null!==(e=readline())&&(e+="\n"),!e)return null
r.input=hr(e,!0)}return r.input.shift()},put_char:function(r,e){null===e||10===e?(l(_(r.output,0)),r.output=[]):0!=e&&r.output.push(e)},flush:function(r){r.output&&r.output.length>0&&(l(_(r.output,0)),r.output=[])}},default_tty1_ops:{put_char:function(r,e){null===e||10===e?(f(_(r.output,0)),r.output=[]):0!=e&&r.output.push(e)},flush:function(r){r.output&&r.output.length>0&&(f(_(r.output,0)),r.output=[])}}},sr={ops_table:null,mount:function(r){return sr.createNode(null,"/",16895,0)},createNode:function(r,e,n,t){if(ur.isBlkdev(n)||ur.isFIFO(n))throw new ur.ErrnoError(63)
sr.ops_table||(sr.ops_table={dir:{node:{getattr:sr.node_ops.getattr,setattr:sr.node_ops.setattr,lookup:sr.node_ops.lookup,mknod:sr.node_ops.mknod,rename:sr.node_ops.rename,unlink:sr.node_ops.unlink,rmdir:sr.node_ops.rmdir,readdir:sr.node_ops.readdir,symlink:sr.node_ops.symlink},stream:{llseek:sr.stream_ops.llseek}},file:{node:{getattr:sr.node_ops.getattr,setattr:sr.node_ops.setattr},stream:{llseek:sr.stream_ops.llseek,read:sr.stream_ops.read,write:sr.stream_ops.write,allocate:sr.stream_ops.allocate,mmap:sr.stream_ops.mmap,msync:sr.stream_ops.msync}},link:{node:{getattr:sr.node_ops.getattr,setattr:sr.node_ops.setattr,readlink:sr.node_ops.readlink},stream:{}},chrdev:{node:{getattr:sr.node_ops.getattr,setattr:sr.node_ops.setattr},stream:ur.chrdev_stream_ops}})
var o=ur.createNode(r,e,n,t)
return ur.isDir(o.mode)?(o.node_ops=sr.ops_table.dir.node,o.stream_ops=sr.ops_table.dir.stream,o.contents={}):ur.isFile(o.mode)?(o.node_ops=sr.ops_table.file.node,o.stream_ops=sr.ops_table.file.stream,o.usedBytes=0,o.contents=null):ur.isLink(o.mode)?(o.node_ops=sr.ops_table.link.node,o.stream_ops=sr.ops_table.link.stream):ur.isChrdev(o.mode)&&(o.node_ops=sr.ops_table.chrdev.node,o.stream_ops=sr.ops_table.chrdev.stream),o.timestamp=Date.now(),r&&(r.contents[e]=o),o},getFileDataAsRegularArray:function(r){if(r.contents&&r.contents.subarray){for(var e=[],n=0;n<r.usedBytes;++n)e.push(r.contents[n])
return e}return r.contents},getFileDataAsTypedArray:function(r){return r.contents?r.contents.subarray?r.contents.subarray(0,r.usedBytes):new Uint8Array(r.contents):new Uint8Array(0)},expandFileStorage:function(r,e){var n=r.contents?r.contents.length:0
if(!(n>=e)){e=Math.max(e,n*(n<1048576?2:1.125)>>>0),0!=n&&(e=Math.max(e,256))
var t=r.contents
r.contents=new Uint8Array(e),r.usedBytes>0&&r.contents.set(t.subarray(0,r.usedBytes),0)}},resizeFileStorage:function(r,e){if(r.usedBytes!=e){if(0==e)return r.contents=null,void(r.usedBytes=0)
if(!r.contents||r.contents.subarray){var n=r.contents
return r.contents=new Uint8Array(e),n&&r.contents.set(n.subarray(0,Math.min(e,r.usedBytes))),void(r.usedBytes=e)}if(r.contents||(r.contents=[]),r.contents.length>e)r.contents.length=e
else for(;r.contents.length<e;)r.contents.push(0)
r.usedBytes=e}},node_ops:{getattr:function(r){var e={}
return e.dev=ur.isChrdev(r.mode)?r.id:1,e.ino=r.id,e.mode=r.mode,e.nlink=1,e.uid=0,e.gid=0,e.rdev=r.rdev,ur.isDir(r.mode)?e.size=4096:ur.isFile(r.mode)?e.size=r.usedBytes:ur.isLink(r.mode)?e.size=r.link.length:e.size=0,e.atime=new Date(r.timestamp),e.mtime=new Date(r.timestamp),e.ctime=new Date(r.timestamp),e.blksize=4096,e.blocks=Math.ceil(e.size/e.blksize),e},setattr:function(r,e){void 0!==e.mode&&(r.mode=e.mode),void 0!==e.timestamp&&(r.timestamp=e.timestamp),void 0!==e.size&&sr.resizeFileStorage(r,e.size)},lookup:function(r,e){throw ur.genericErrors[44]},mknod:function(r,e,n,t){return sr.createNode(r,e,n,t)},rename:function(r,e,n){if(ur.isDir(r.mode)){var t
try{t=ur.lookupNode(e,n)}catch(r){}if(t)for(var o in t.contents)throw new ur.ErrnoError(55)}delete r.parent.contents[r.name],r.name=n,e.contents[n]=r,r.parent=e},unlink:function(r,e){delete r.contents[e]},rmdir:function(r,e){var n=ur.lookupNode(r,e)
for(var t in n.contents)throw new ur.ErrnoError(55)
delete r.contents[e]},readdir:function(r){var e=[".",".."]
for(var n in r.contents)r.contents.hasOwnProperty(n)&&e.push(n)
return e},symlink:function(r,e,n){var t=sr.createNode(r,e,41471,0)
return t.link=n,t},readlink:function(r){if(!ur.isLink(r.mode))throw new ur.ErrnoError(28)
return r.link}},stream_ops:{read:function(r,e,n,t,o){var i=r.node.contents
if(o>=r.node.usedBytes)return 0
var a=Math.min(r.node.usedBytes-o,t)
if(a>8&&i.subarray)e.set(i.subarray(o,o+a),n)
else for(var s=0;s<a;s++)e[n+s]=i[o+s]
return a},write:function(r,e,n,t,o,i){if(!t)return 0
var a=r.node
if(a.timestamp=Date.now(),e.subarray&&(!a.contents||a.contents.subarray)){if(i)return a.contents=e.subarray(n,n+t),a.usedBytes=t,t
if(0===a.usedBytes&&0===o)return a.contents=e.slice(n,n+t),a.usedBytes=t,t
if(o+t<=a.usedBytes)return a.contents.set(e.subarray(n,n+t),o),t}if(sr.expandFileStorage(a,o+t),a.contents.subarray&&e.subarray)a.contents.set(e.subarray(n,n+t),o)
else for(var s=0;s<t;s++)a.contents[o+s]=e[n+s]
return a.usedBytes=Math.max(a.usedBytes,o+t),t},llseek:function(r,e,n){var t=e
if(1===n?t+=r.position:2===n&&ur.isFile(r.node.mode)&&(t+=r.node.usedBytes),t<0)throw new ur.ErrnoError(28)
return t},allocate:function(r,e,n){sr.expandFileStorage(r.node,e+n),r.node.usedBytes=Math.max(r.node.usedBytes,e+n)},mmap:function(r,e,n,t,o,i,a){if(!ur.isFile(r.node.mode))throw new ur.ErrnoError(43)
var s,u,c=r.node.contents
if(2&a||c.buffer!==e.buffer){(o>0||o+t<c.length)&&(c=c.subarray?c.subarray(o,o+t):Array.prototype.slice.call(c,o,o+t)),u=!0
var l=e.buffer==P.buffer
if(!(s=Er(t)))throw new ur.ErrnoError(48);(l?P:e).set(c,s)}else u=!1,s=c.byteOffset
return{ptr:s,allocated:u}},msync:function(r,e,n,t,o){if(!ur.isFile(r.node.mode))throw new ur.ErrnoError(43)
if(2&o)return 0
sr.stream_ops.write(r,e,0,t,n,!1)
return 0}}},ur={root:null,mounts:[],devices:{},streams:[],nextInode:1,nameTable:null,currentPath:"/",initialized:!1,ignorePermissions:!0,trackingDelegate:{},tracking:{openFlags:{READ:1,WRITE:2}},ErrnoError:null,genericErrors:{},filesystems:null,syncFSRequests:0,handleFSError:function(r){if(!(r instanceof ur.ErrnoError))throw r+" : "+nr()
return tr(r.errno)},lookupPath:function(r,e){if(e=e||{},!(r=ir.resolve(ur.cwd(),r)))return{path:"",node:null}
var n={follow_mount:!0,recurse_count:0}
for(var t in n)void 0===e[t]&&(e[t]=n[t])
if(e.recurse_count>8)throw new ur.ErrnoError(32)
for(var o=or.normalizeArray(r.split("/").filter((function(r){return!!r})),!1),i=ur.root,a="/",s=0;s<o.length;s++){var u=s===o.length-1
if(u&&e.parent)break
if(i=ur.lookupNode(i,o[s]),a=or.join2(a,o[s]),ur.isMountpoint(i)&&(!u||u&&e.follow_mount)&&(i=i.mounted.root),!u||e.follow)for(var c=0;ur.isLink(i.mode);){var l=ur.readlink(a)
if(a=ir.resolve(or.dirname(a),l),i=ur.lookupPath(a,{recurse_count:e.recurse_count}).node,c++>40)throw new ur.ErrnoError(32)}}return{path:a,node:i}},getPath:function(r){for(var e;;){if(ur.isRoot(r)){var n=r.mount.mountpoint
return e?"/"!==n[n.length-1]?n+"/"+e:n+e:n}e=e?r.name+"/"+e:r.name,r=r.parent}},hashName:function(r,e){for(var n=0,t=0;t<e.length;t++)n=(n<<5)-n+e.charCodeAt(t)|0
return(r+n>>>0)%ur.nameTable.length},hashAddNode:function(r){var e=ur.hashName(r.parent.id,r.name)
r.name_next=ur.nameTable[e],ur.nameTable[e]=r},hashRemoveNode:function(r){var e=ur.hashName(r.parent.id,r.name)
if(ur.nameTable[e]===r)ur.nameTable[e]=r.name_next
else for(var n=ur.nameTable[e];n;){if(n.name_next===r){n.name_next=r.name_next
break}n=n.name_next}},lookupNode:function(r,e){var n=ur.mayLookup(r)
if(n)throw new ur.ErrnoError(n,r)
for(var t=ur.hashName(r.id,e),o=ur.nameTable[t];o;o=o.name_next){var i=o.name
if(o.parent.id===r.id&&i===e)return o}return ur.lookup(r,e)},createNode:function(r,e,n,t){var o=new ur.FSNode(r,e,n,t)
return ur.hashAddNode(o),o},destroyNode:function(r){ur.hashRemoveNode(r)},isRoot:function(r){return r===r.parent},isMountpoint:function(r){return!!r.mounted},isFile:function(r){return 32768==(61440&r)},isDir:function(r){return 16384==(61440&r)},isLink:function(r){return 40960==(61440&r)},isChrdev:function(r){return 8192==(61440&r)},isBlkdev:function(r){return 24576==(61440&r)},isFIFO:function(r){return 4096==(61440&r)},isSocket:function(r){return 49152==(49152&r)},flagModes:{r:0,rs:1052672,"r+":2,w:577,wx:705,xw:705,"w+":578,"wx+":706,"xw+":706,a:1089,ax:1217,xa:1217,"a+":1090,"ax+":1218,"xa+":1218},modeStringToFlags:function(r){var e=ur.flagModes[r]
if(void 0===e)throw new Error("Unknown file open mode: "+r)
return e},flagsToPermissionString:function(r){var e=["r","w","rw"][3&r]
return 512&r&&(e+="w"),e},nodePermissions:function(r,e){return ur.ignorePermissions||(-1===e.indexOf("r")||292&r.mode)&&(-1===e.indexOf("w")||146&r.mode)&&(-1===e.indexOf("x")||73&r.mode)?0:2},mayLookup:function(r){var e=ur.nodePermissions(r,"x")
return e||(r.node_ops.lookup?0:2)},mayCreate:function(r,e){try{ur.lookupNode(r,e)
return 20}catch(r){}return ur.nodePermissions(r,"wx")},mayDelete:function(r,e,n){var t
try{t=ur.lookupNode(r,e)}catch(r){return r.errno}var o=ur.nodePermissions(r,"wx")
if(o)return o
if(n){if(!ur.isDir(t.mode))return 54
if(ur.isRoot(t)||ur.getPath(t)===ur.cwd())return 10}else if(ur.isDir(t.mode))return 31
return 0},mayOpen:function(r,e){return r?ur.isLink(r.mode)?32:ur.isDir(r.mode)&&("r"!==ur.flagsToPermissionString(e)||512&e)?31:ur.nodePermissions(r,ur.flagsToPermissionString(e)):44},MAX_OPEN_FDS:4096,nextfd:function(r,e){r=r||0,e=e||ur.MAX_OPEN_FDS
for(var n=r;n<=e;n++)if(!ur.streams[n])return n
throw new ur.ErrnoError(33)},getStream:function(r){return ur.streams[r]},createStream:function(r,e,n){ur.FSStream||(ur.FSStream=function(){},ur.FSStream.prototype={object:{get:function(){return this.node},set:function(r){this.node=r}},isRead:{get:function(){return 1!=(2097155&this.flags)}},isWrite:{get:function(){return 0!=(2097155&this.flags)}},isAppend:{get:function(){return 1024&this.flags}}})
var t=new ur.FSStream
for(var o in r)t[o]=r[o]
r=t
var i=ur.nextfd(e,n)
return r.fd=i,ur.streams[i]=r,r},closeStream:function(r){ur.streams[r]=null},chrdev_stream_ops:{open:function(r){var e=ur.getDevice(r.node.rdev)
r.stream_ops=e.stream_ops,r.stream_ops.open&&r.stream_ops.open(r)},llseek:function(){throw new ur.ErrnoError(70)}},major:function(r){return r>>8},minor:function(r){return 255&r},makedev:function(r,e){return r<<8|e},registerDevice:function(r,e){ur.devices[r]={stream_ops:e}},getDevice:function(r){return ur.devices[r]},getMounts:function(r){for(var e=[],n=[r];n.length;){var t=n.pop()
e.push(t),n.push.apply(n,t.mounts)}return e},syncfs:function(r,e){"function"==typeof r&&(e=r,r=!1),ur.syncFSRequests++,ur.syncFSRequests>1&&f("warning: "+ur.syncFSRequests+" FS.syncfs operations in flight at once, probably just doing extra work")
var n=ur.getMounts(ur.root.mount),t=0
function o(r){return ur.syncFSRequests--,e(r)}function i(r){if(r)return i.errored?void 0:(i.errored=!0,o(r));++t>=n.length&&o(null)}n.forEach((function(e){if(!e.type.syncfs)return i(null)
e.type.syncfs(e,r,i)}))},mount:function(r,e,n){var t,o="/"===n,i=!n
if(o&&ur.root)throw new ur.ErrnoError(10)
if(!o&&!i){var a=ur.lookupPath(n,{follow_mount:!1})
if(n=a.path,t=a.node,ur.isMountpoint(t))throw new ur.ErrnoError(10)
if(!ur.isDir(t.mode))throw new ur.ErrnoError(54)}var s={type:r,opts:e,mountpoint:n,mounts:[]},u=r.mount(s)
return u.mount=s,s.root=u,o?ur.root=u:t&&(t.mounted=s,t.mount&&t.mount.mounts.push(s)),u},unmount:function(r){var e=ur.lookupPath(r,{follow_mount:!1})
if(!ur.isMountpoint(e.node))throw new ur.ErrnoError(28)
var n=e.node,t=n.mounted,o=ur.getMounts(t)
Object.keys(ur.nameTable).forEach((function(r){for(var e=ur.nameTable[r];e;){var n=e.name_next;-1!==o.indexOf(e.mount)&&ur.destroyNode(e),e=n}})),n.mounted=null
var i=n.mount.mounts.indexOf(t)
n.mount.mounts.splice(i,1)},lookup:function(r,e){return r.node_ops.lookup(r,e)},mknod:function(r,e,n){var t=ur.lookupPath(r,{parent:!0}).node,o=or.basename(r)
if(!o||"."===o||".."===o)throw new ur.ErrnoError(28)
var i=ur.mayCreate(t,o)
if(i)throw new ur.ErrnoError(i)
if(!t.node_ops.mknod)throw new ur.ErrnoError(63)
return t.node_ops.mknod(t,o,e,n)},create:function(r,e){return e=void 0!==e?e:438,e&=4095,e|=32768,ur.mknod(r,e,0)},mkdir:function(r,e){return e=void 0!==e?e:511,e&=1023,e|=16384,ur.mknod(r,e,0)},mkdirTree:function(r,e){for(var n=r.split("/"),t="",o=0;o<n.length;++o)if(n[o]){t+="/"+n[o]
try{ur.mkdir(t,e)}catch(r){if(20!=r.errno)throw r}}},mkdev:function(r,e,n){return void 0===n&&(n=e,e=438),e|=8192,ur.mknod(r,e,n)},symlink:function(r,e){if(!ir.resolve(r))throw new ur.ErrnoError(44)
var n=ur.lookupPath(e,{parent:!0}).node
if(!n)throw new ur.ErrnoError(44)
var t=or.basename(e),o=ur.mayCreate(n,t)
if(o)throw new ur.ErrnoError(o)
if(!n.node_ops.symlink)throw new ur.ErrnoError(63)
return n.node_ops.symlink(n,t,r)},rename:function(r,e){var n,t,o=or.dirname(r),i=or.dirname(e),a=or.basename(r),s=or.basename(e)
try{n=ur.lookupPath(r,{parent:!0}).node,t=ur.lookupPath(e,{parent:!0}).node}catch(r){throw new ur.ErrnoError(10)}if(!n||!t)throw new ur.ErrnoError(44)
if(n.mount!==t.mount)throw new ur.ErrnoError(75)
var u,c=ur.lookupNode(n,a),l=ir.relative(r,i)
if("."!==l.charAt(0))throw new ur.ErrnoError(28)
if("."!==(l=ir.relative(e,o)).charAt(0))throw new ur.ErrnoError(55)
try{u=ur.lookupNode(t,s)}catch(r){}if(c!==u){var d=ur.isDir(c.mode),m=ur.mayDelete(n,a,d)
if(m)throw new ur.ErrnoError(m)
if(m=u?ur.mayDelete(t,s,d):ur.mayCreate(t,s))throw new ur.ErrnoError(m)
if(!n.node_ops.rename)throw new ur.ErrnoError(63)
if(ur.isMountpoint(c)||u&&ur.isMountpoint(u))throw new ur.ErrnoError(10)
if(t!==n&&(m=ur.nodePermissions(n,"w")))throw new ur.ErrnoError(m)
try{ur.trackingDelegate.willMovePath&&ur.trackingDelegate.willMovePath(r,e)}catch(n){f("FS.trackingDelegate['willMovePath']('"+r+"', '"+e+"') threw an exception: "+n.message)}ur.hashRemoveNode(c)
try{n.node_ops.rename(c,t,s)}catch(r){throw r}finally{ur.hashAddNode(c)}try{ur.trackingDelegate.onMovePath&&ur.trackingDelegate.onMovePath(r,e)}catch(n){f("FS.trackingDelegate['onMovePath']('"+r+"', '"+e+"') threw an exception: "+n.message)}}},rmdir:function(r){var e=ur.lookupPath(r,{parent:!0}).node,n=or.basename(r),t=ur.lookupNode(e,n),o=ur.mayDelete(e,n,!0)
if(o)throw new ur.ErrnoError(o)
if(!e.node_ops.rmdir)throw new ur.ErrnoError(63)
if(ur.isMountpoint(t))throw new ur.ErrnoError(10)
try{ur.trackingDelegate.willDeletePath&&ur.trackingDelegate.willDeletePath(r)}catch(e){f("FS.trackingDelegate['willDeletePath']('"+r+"') threw an exception: "+e.message)}e.node_ops.rmdir(e,n),ur.destroyNode(t)
try{ur.trackingDelegate.onDeletePath&&ur.trackingDelegate.onDeletePath(r)}catch(e){f("FS.trackingDelegate['onDeletePath']('"+r+"') threw an exception: "+e.message)}},readdir:function(r){var e=ur.lookupPath(r,{follow:!0}).node
if(!e.node_ops.readdir)throw new ur.ErrnoError(54)
return e.node_ops.readdir(e)},unlink:function(r){var e=ur.lookupPath(r,{parent:!0}).node,n=or.basename(r),t=ur.lookupNode(e,n),o=ur.mayDelete(e,n,!1)
if(o)throw new ur.ErrnoError(o)
if(!e.node_ops.unlink)throw new ur.ErrnoError(63)
if(ur.isMountpoint(t))throw new ur.ErrnoError(10)
try{ur.trackingDelegate.willDeletePath&&ur.trackingDelegate.willDeletePath(r)}catch(e){f("FS.trackingDelegate['willDeletePath']('"+r+"') threw an exception: "+e.message)}e.node_ops.unlink(e,n),ur.destroyNode(t)
try{ur.trackingDelegate.onDeletePath&&ur.trackingDelegate.onDeletePath(r)}catch(e){f("FS.trackingDelegate['onDeletePath']('"+r+"') threw an exception: "+e.message)}},readlink:function(r){var e=ur.lookupPath(r).node
if(!e)throw new ur.ErrnoError(44)
if(!e.node_ops.readlink)throw new ur.ErrnoError(28)
return ir.resolve(ur.getPath(e.parent),e.node_ops.readlink(e))},stat:function(r,e){var n=ur.lookupPath(r,{follow:!e}).node
if(!n)throw new ur.ErrnoError(44)
if(!n.node_ops.getattr)throw new ur.ErrnoError(63)
return n.node_ops.getattr(n)},lstat:function(r){return ur.stat(r,!0)},chmod:function(r,e,n){var t
"string"==typeof r?t=ur.lookupPath(r,{follow:!n}).node:t=r
if(!t.node_ops.setattr)throw new ur.ErrnoError(63)
t.node_ops.setattr(t,{mode:4095&e|-4096&t.mode,timestamp:Date.now()})},lchmod:function(r,e){ur.chmod(r,e,!0)},fchmod:function(r,e){var n=ur.getStream(r)
if(!n)throw new ur.ErrnoError(8)
ur.chmod(n.node,e)},chown:function(r,e,n,t){var o
"string"==typeof r?o=ur.lookupPath(r,{follow:!t}).node:o=r
if(!o.node_ops.setattr)throw new ur.ErrnoError(63)
o.node_ops.setattr(o,{timestamp:Date.now()})},lchown:function(r,e,n){ur.chown(r,e,n,!0)},fchown:function(r,e,n){var t=ur.getStream(r)
if(!t)throw new ur.ErrnoError(8)
ur.chown(t.node,e,n)},truncate:function(r,e){if(e<0)throw new ur.ErrnoError(28)
var n
"string"==typeof r?n=ur.lookupPath(r,{follow:!0}).node:n=r
if(!n.node_ops.setattr)throw new ur.ErrnoError(63)
if(ur.isDir(n.mode))throw new ur.ErrnoError(31)
if(!ur.isFile(n.mode))throw new ur.ErrnoError(28)
var t=ur.nodePermissions(n,"w")
if(t)throw new ur.ErrnoError(t)
n.node_ops.setattr(n,{size:e,timestamp:Date.now()})},ftruncate:function(r,e){var n=ur.getStream(r)
if(!n)throw new ur.ErrnoError(8)
if(0==(2097155&n.flags))throw new ur.ErrnoError(28)
ur.truncate(n.node,e)},utime:function(r,e,n){var t=ur.lookupPath(r,{follow:!0}).node
t.node_ops.setattr(t,{timestamp:Math.max(e,n)})},open:function(e,n,t,o,i){if(""===e)throw new ur.ErrnoError(44)
var a
if(t=void 0===t?438:t,t=64&(n="string"==typeof n?ur.modeStringToFlags(n):n)?4095&t|32768:0,"object"==typeof e)a=e
else{e=or.normalize(e)
try{a=ur.lookupPath(e,{follow:!(131072&n)}).node}catch(r){}}var s=!1
if(64&n)if(a){if(128&n)throw new ur.ErrnoError(20)}else a=ur.mknod(e,t,0),s=!0
if(!a)throw new ur.ErrnoError(44)
if(ur.isChrdev(a.mode)&&(n&=-513),65536&n&&!ur.isDir(a.mode))throw new ur.ErrnoError(54)
if(!s){var u=ur.mayOpen(a,n)
if(u)throw new ur.ErrnoError(u)}512&n&&ur.truncate(a,0),n&=-131713
var c=ur.createStream({node:a,path:ur.getPath(a),flags:n,seekable:!0,position:0,stream_ops:a.stream_ops,ungotten:[],error:!1},o,i)
c.stream_ops.open&&c.stream_ops.open(c),!r.logReadFiles||1&n||(ur.readFiles||(ur.readFiles={}),e in ur.readFiles||(ur.readFiles[e]=1,f("FS.trackingDelegate error on read file: "+e)))
try{if(ur.trackingDelegate.onOpenFile){var l=0
1!=(2097155&n)&&(l|=ur.tracking.openFlags.READ),0!=(2097155&n)&&(l|=ur.tracking.openFlags.WRITE),ur.trackingDelegate.onOpenFile(e,l)}}catch(r){f("FS.trackingDelegate['onOpenFile']('"+e+"', flags) threw an exception: "+r.message)}return c},close:function(r){if(ur.isClosed(r))throw new ur.ErrnoError(8)
r.getdents&&(r.getdents=null)
try{r.stream_ops.close&&r.stream_ops.close(r)}catch(r){throw r}finally{ur.closeStream(r.fd)}r.fd=null},isClosed:function(r){return null===r.fd},llseek:function(r,e,n){if(ur.isClosed(r))throw new ur.ErrnoError(8)
if(!r.seekable||!r.stream_ops.llseek)throw new ur.ErrnoError(70)
if(0!=n&&1!=n&&2!=n)throw new ur.ErrnoError(28)
return r.position=r.stream_ops.llseek(r,e,n),r.ungotten=[],r.position},read:function(r,e,n,t,o){if(t<0||o<0)throw new ur.ErrnoError(28)
if(ur.isClosed(r))throw new ur.ErrnoError(8)
if(1==(2097155&r.flags))throw new ur.ErrnoError(8)
if(ur.isDir(r.node.mode))throw new ur.ErrnoError(31)
if(!r.stream_ops.read)throw new ur.ErrnoError(28)
var i=void 0!==o
if(i){if(!r.seekable)throw new ur.ErrnoError(70)}else o=r.position
var a=r.stream_ops.read(r,e,n,t,o)
return i||(r.position+=a),a},write:function(r,e,n,t,o,i){if(t<0||o<0)throw new ur.ErrnoError(28)
if(ur.isClosed(r))throw new ur.ErrnoError(8)
if(0==(2097155&r.flags))throw new ur.ErrnoError(8)
if(ur.isDir(r.node.mode))throw new ur.ErrnoError(31)
if(!r.stream_ops.write)throw new ur.ErrnoError(28)
r.seekable&&1024&r.flags&&ur.llseek(r,0,2)
var a=void 0!==o
if(a){if(!r.seekable)throw new ur.ErrnoError(70)}else o=r.position
var s=r.stream_ops.write(r,e,n,t,o,i)
a||(r.position+=s)
try{r.path&&ur.trackingDelegate.onWriteToFile&&ur.trackingDelegate.onWriteToFile(r.path)}catch(e){f("FS.trackingDelegate['onWriteToFile']('"+r.path+"') threw an exception: "+e.message)}return s},allocate:function(r,e,n){if(ur.isClosed(r))throw new ur.ErrnoError(8)
if(e<0||n<=0)throw new ur.ErrnoError(28)
if(0==(2097155&r.flags))throw new ur.ErrnoError(8)
if(!ur.isFile(r.node.mode)&&!ur.isDir(r.node.mode))throw new ur.ErrnoError(43)
if(!r.stream_ops.allocate)throw new ur.ErrnoError(138)
r.stream_ops.allocate(r,e,n)},mmap:function(r,e,n,t,o,i,a){if(0!=(2&i)&&0==(2&a)&&2!=(2097155&r.flags))throw new ur.ErrnoError(2)
if(1==(2097155&r.flags))throw new ur.ErrnoError(2)
if(!r.stream_ops.mmap)throw new ur.ErrnoError(43)
return r.stream_ops.mmap(r,e,n,t,o,i,a)},msync:function(r,e,n,t,o){return r&&r.stream_ops.msync?r.stream_ops.msync(r,e,n,t,o):0},munmap:function(r){return 0},ioctl:function(r,e,n){if(!r.stream_ops.ioctl)throw new ur.ErrnoError(59)
return r.stream_ops.ioctl(r,e,n)},readFile:function(r,e){if((e=e||{}).flags=e.flags||"r",e.encoding=e.encoding||"binary","utf8"!==e.encoding&&"binary"!==e.encoding)throw new Error('Invalid encoding type "'+e.encoding+'"')
var n,t=ur.open(r,e.flags),o=ur.stat(r).size,i=new Uint8Array(o)
return ur.read(t,i,0,o,0),"utf8"===e.encoding?n=_(i,0):"binary"===e.encoding&&(n=i),ur.close(t),n},writeFile:function(r,e,n){(n=n||{}).flags=n.flags||"w"
var t=ur.open(r,n.flags,n.mode)
if("string"==typeof e){var o=new Uint8Array(S(e)+1),i=b(e,o,0,o.length)
ur.write(t,o,0,i,void 0,n.canOwn)}else{if(!ArrayBuffer.isView(e))throw new Error("Unsupported data type")
ur.write(t,e,0,e.byteLength,void 0,n.canOwn)}ur.close(t)},cwd:function(){return ur.currentPath},chdir:function(r){var e=ur.lookupPath(r,{follow:!0})
if(null===e.node)throw new ur.ErrnoError(44)
if(!ur.isDir(e.node.mode))throw new ur.ErrnoError(54)
var n=ur.nodePermissions(e.node,"x")
if(n)throw new ur.ErrnoError(n)
ur.currentPath=e.path},createDefaultDirectories:function(){ur.mkdir("/tmp"),ur.mkdir("/home"),ur.mkdir("/home/web_user")},createDefaultDevices:function(){var r
if(ur.mkdir("/dev"),ur.registerDevice(ur.makedev(1,3),{read:function(){return 0},write:function(r,e,n,t,o){return t}}),ur.mkdev("/dev/null",ur.makedev(1,3)),ar.register(ur.makedev(5,0),ar.default_tty_ops),ar.register(ur.makedev(6,0),ar.default_tty1_ops),ur.mkdev("/dev/tty",ur.makedev(5,0)),ur.mkdev("/dev/tty1",ur.makedev(6,0)),"object"==typeof crypto&&"function"==typeof crypto.getRandomValues){var e=new Uint8Array(1)
r=function(){return crypto.getRandomValues(e),e[0]}}r||(r=function(){G("random_device")}),ur.createDevice("/dev","random",r),ur.createDevice("/dev","urandom",r),ur.mkdir("/dev/shm"),ur.mkdir("/dev/shm/tmp")},createSpecialDirectories:function(){ur.mkdir("/proc"),ur.mkdir("/proc/self"),ur.mkdir("/proc/self/fd"),ur.mount({mount:function(){var r=ur.createNode("/proc/self","fd",16895,73)
return r.node_ops={lookup:function(r,e){var n=+e,t=ur.getStream(n)
if(!t)throw new ur.ErrnoError(8)
var o={parent:null,mount:{mountpoint:"fake"},node_ops:{readlink:function(){return t.path}}}
return o.parent=o,o}},r}},{},"/proc/self/fd")},createStandardStreams:function(){r.stdin?ur.createDevice("/dev","stdin",r.stdin):ur.symlink("/dev/tty","/dev/stdin"),r.stdout?ur.createDevice("/dev","stdout",null,r.stdout):ur.symlink("/dev/tty","/dev/stdout"),r.stderr?ur.createDevice("/dev","stderr",null,r.stderr):ur.symlink("/dev/tty1","/dev/stderr")
ur.open("/dev/stdin","r"),ur.open("/dev/stdout","w"),ur.open("/dev/stderr","w")},ensureErrnoError:function(){ur.ErrnoError||(ur.ErrnoError=function(r,e){this.node=e,this.setErrno=function(r){this.errno=r},this.setErrno(r),this.message="FS error"},ur.ErrnoError.prototype=new Error,ur.ErrnoError.prototype.constructor=ur.ErrnoError,[44].forEach((function(r){ur.genericErrors[r]=new ur.ErrnoError(r),ur.genericErrors[r].stack="<generic error, no stack>"})))},staticInit:function(){ur.ensureErrnoError(),ur.nameTable=new Array(4096),ur.mount(sr,{},"/"),ur.createDefaultDirectories(),ur.createDefaultDevices(),ur.createSpecialDirectories(),ur.filesystems={MEMFS:sr}},init:function(e,n,t){ur.init.initialized=!0,ur.ensureErrnoError(),r.stdin=e||r.stdin,r.stdout=n||r.stdout,r.stderr=t||r.stderr,ur.createStandardStreams()},quit:function(){ur.init.initialized=!1
var e=r._fflush
e&&e(0)
for(var n=0;n<ur.streams.length;n++){var t=ur.streams[n]
t&&ur.close(t)}},getMode:function(r,e){var n=0
return r&&(n|=365),e&&(n|=146),n},joinPath:function(r,e){var n=or.join.apply(null,r)
return e&&"/"==n[0]&&(n=n.substr(1)),n},absolutePath:function(r,e){return ir.resolve(e,r)},standardizePath:function(r){return or.normalize(r)},findObject:function(r,e){var n=ur.analyzePath(r,e)
return n.exists?n.object:(tr(n.error),null)},analyzePath:function(r,e){try{r=(t=ur.lookupPath(r,{follow:!e})).path}catch(r){}var n={isRoot:!1,exists:!1,error:0,name:null,path:null,object:null,parentExists:!1,parentPath:null,parentObject:null}
try{var t=ur.lookupPath(r,{parent:!0})
n.parentExists=!0,n.parentPath=t.path,n.parentObject=t.node,n.name=or.basename(r),t=ur.lookupPath(r,{follow:!e}),n.exists=!0,n.path=t.path,n.object=t.node,n.name=t.node.name,n.isRoot="/"===t.path}catch(r){n.error=r.errno}return n},createFolder:function(r,e,n,t){var o=or.join2("string"==typeof r?r:ur.getPath(r),e),i=ur.getMode(n,t)
return ur.mkdir(o,i)},createPath:function(r,e,n,t){r="string"==typeof r?r:ur.getPath(r)
for(var o=e.split("/").reverse();o.length;){var i=o.pop()
if(i){var a=or.join2(r,i)
try{ur.mkdir(a)}catch(r){}r=a}}return a},createFile:function(r,e,n,t,o){var i=or.join2("string"==typeof r?r:ur.getPath(r),e),a=ur.getMode(t,o)
return ur.create(i,a)},createDataFile:function(r,e,n,t,o,i){var a=e?or.join2("string"==typeof r?r:ur.getPath(r),e):r,s=ur.getMode(t,o),u=ur.create(a,s)
if(n){if("string"==typeof n){for(var c=new Array(n.length),l=0,f=n.length;l<f;++l)c[l]=n.charCodeAt(l)
n=c}ur.chmod(u,146|s)
var d=ur.open(u,"w")
ur.write(d,n,0,n.length,0,i),ur.close(d),ur.chmod(u,s)}return u},createDevice:function(r,e,n,t){var o=or.join2("string"==typeof r?r:ur.getPath(r),e),i=ur.getMode(!!n,!!t)
ur.createDevice.major||(ur.createDevice.major=64)
var a=ur.makedev(ur.createDevice.major++,0)
return ur.registerDevice(a,{open:function(r){r.seekable=!1},close:function(r){t&&t.buffer&&t.buffer.length&&t(10)},read:function(r,e,t,o,i){for(var a=0,s=0;s<o;s++){var u
try{u=n()}catch(r){throw new ur.ErrnoError(29)}if(void 0===u&&0===a)throw new ur.ErrnoError(6)
if(null==u)break
a++,e[t+s]=u}return a&&(r.node.timestamp=Date.now()),a},write:function(r,e,n,o,i){for(var a=0;a<o;a++)try{t(e[n+a])}catch(r){throw new ur.ErrnoError(29)}return o&&(r.node.timestamp=Date.now()),a}}),ur.mkdev(o,i,a)},createLink:function(r,e,n,t,o){var i=or.join2("string"==typeof r?r:ur.getPath(r),e)
return ur.symlink(n,i)},forceLoadFile:function(r){if(r.isDevice||r.isFolder||r.link||r.contents)return!0
var e=!0
if("undefined"!=typeof XMLHttpRequest)throw new Error("Lazy loading should have been performed (contents set) in createLazyFile, but it was not. Lazy loading only works in web workers. Use --embed-file or --preload-file in emcc on the main thread.")
if(!s)throw new Error("Cannot load without read() or XMLHttpRequest.")
try{r.contents=hr(s(r.url),!0),r.usedBytes=r.contents.length}catch(r){e=!1}return e||tr(29),e},createLazyFile:function(r,e,n,t,i){function a(){this.lengthKnown=!1,this.chunks=[]}if(a.prototype.get=function(r){if(!(r>this.length-1||r<0)){var e=r%this.chunkSize,n=r/this.chunkSize|0
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
return void 0!==o.response?new Uint8Array(o.response||[]):hr(o.responseText||"",!0)}(e,o)),void 0===s.chunks[r])throw new Error("doXHR failed!")
return s.chunks[r]})),!i&&t||(a=t=1,t=this.getter(0).length,a=t,l("LazyFiles on gzip forces download of the whole file when length is accessed")),this._length=t,this._chunkSize=a,this.lengthKnown=!0},"undefined"!=typeof XMLHttpRequest){if(!o)throw"Cannot do synchronous binary XHRs outside webworkers in modern browsers. Use --embed-file or --preload-file in emcc"
var s=new a
Object.defineProperties(s,{length:{get:function(){return this.lengthKnown||this.cacheLength(),this._length}},chunkSize:{get:function(){return this.lengthKnown||this.cacheLength(),this._chunkSize}}})
var u={isDevice:!1,contents:s}}else u={isDevice:!1,url:n}
var c=ur.createFile(r,e,u,t,i)
u.contents?c.contents=u.contents:u.url&&(c.contents=null,c.url=u.url),Object.defineProperties(c,{usedBytes:{get:function(){return this.contents.length}}})
var f={}
return Object.keys(c.stream_ops).forEach((function(r){var e=c.stream_ops[r]
f[r]=function(){if(!ur.forceLoadFile(c))throw new ur.ErrnoError(29)
return e.apply(null,arguments)}})),f.read=function(r,e,n,t,o){if(!ur.forceLoadFile(c))throw new ur.ErrnoError(29)
var i=r.node.contents
if(o>=i.length)return 0
var a=Math.min(i.length-o,t)
if(i.slice)for(var s=0;s<a;s++)e[n+s]=i[o+s]
else for(s=0;s<a;s++)e[n+s]=i.get(o+s)
return a},c.stream_ops=f,c},createPreloadedFile:function(e,n,t,o,i,a,s,u,c,l){Browser.init()
var f=n?ir.resolve(or.join2(e,n)):e
function d(t){function d(r){l&&l(),u||ur.createDataFile(e,n,r,o,i,c),a&&a(),X()}var m=!1
r.preloadPlugins.forEach((function(r){m||r.canHandle(f)&&(r.handle(t,f,d,(function(){s&&s(),X()})),m=!0)})),m||d(t)}V(),"string"==typeof t?Browser.asyncLoad(t,(function(r){d(r)}),s):d(t)},indexedDB:function(){return window.indexedDB||window.mozIndexedDB||window.webkitIndexedDB||window.msIndexedDB},DB_NAME:function(){return"EM_FS_"+window.location.pathname},DB_VERSION:20,DB_STORE_NAME:"FILE_DATA",saveFilesToDB:function(r,e,n){e=e||function(){},n=n||function(){}
var t=ur.indexedDB()
try{var o=t.open(ur.DB_NAME(),ur.DB_VERSION)}catch(r){return n(r)}o.onupgradeneeded=function(){l("creating db"),o.result.createObjectStore(ur.DB_STORE_NAME)},o.onsuccess=function(){var t=o.result.transaction([ur.DB_STORE_NAME],"readwrite"),i=t.objectStore(ur.DB_STORE_NAME),a=0,s=0,u=r.length
function c(){0==s?e():n()}r.forEach((function(r){var e=i.put(ur.analyzePath(r).object.contents,r)
e.onsuccess=function(){++a+s==u&&c()},e.onerror=function(){s++,a+s==u&&c()}})),t.onerror=n},o.onerror=n},loadFilesFromDB:function(r,e,n){e=e||function(){},n=n||function(){}
var t=ur.indexedDB()
try{var o=t.open(ur.DB_NAME(),ur.DB_VERSION)}catch(r){return n(r)}o.onupgradeneeded=n,o.onsuccess=function(){var t=o.result
try{var i=t.transaction([ur.DB_STORE_NAME],"readonly")}catch(r){return void n(r)}var a=i.objectStore(ur.DB_STORE_NAME),s=0,u=0,c=r.length
function l(){0==u?e():n()}r.forEach((function(r){var e=a.get(r)
e.onsuccess=function(){ur.analyzePath(r).exists&&ur.unlink(r),ur.createDataFile(or.dirname(r),or.basename(r),e.result,!0,!0,!0),++s+u==c&&l()},e.onerror=function(){u++,s+u==c&&l()}})),i.onerror=n},o.onerror=n}},cr={mappings:{},DEFAULT_POLLMASK:5,umask:511,calculateAt:function(r,e){if("/"!==e[0]){var n
if(-100===r)n=ur.cwd()
else{var t=ur.getStream(r)
if(!t)throw new ur.ErrnoError(8)
n=t.path}e=or.join2(n,e)}return e},doStat:function(r,e,n){try{var t=r(e)}catch(r){if(r&&r.node&&or.normalize(e)!==or.normalize(ur.getPath(r.node)))return-54
throw r}return M[n>>2]=t.dev,M[n+4>>2]=0,M[n+8>>2]=t.ino,M[n+12>>2]=t.mode,M[n+16>>2]=t.nlink,M[n+20>>2]=t.uid,M[n+24>>2]=t.gid,M[n+28>>2]=t.rdev,M[n+32>>2]=0,$=[t.size>>>0,(Z=t.size,+L(Z)>=1?Z>0?(0|H(+U(Z/4294967296),4294967295))>>>0:~~+I((Z-+(~~Z>>>0))/4294967296)>>>0:0)],M[n+40>>2]=$[0],M[n+44>>2]=$[1],M[n+48>>2]=4096,M[n+52>>2]=t.blocks,M[n+56>>2]=t.atime.getTime()/1e3|0,M[n+60>>2]=0,M[n+64>>2]=t.mtime.getTime()/1e3|0,M[n+68>>2]=0,M[n+72>>2]=t.ctime.getTime()/1e3|0,M[n+76>>2]=0,$=[t.ino>>>0,(Z=t.ino,+L(Z)>=1?Z>0?(0|H(+U(Z/4294967296),4294967295))>>>0:~~+I((Z-+(~~Z>>>0))/4294967296)>>>0:0)],M[n+80>>2]=$[0],M[n+84>>2]=$[1],0},doMsync:function(r,e,n,t,o){var i=A.slice(r,r+n)
ur.msync(e,i,o,n,t)},doMkdir:function(r,e){return"/"===(r=or.normalize(r))[r.length-1]&&(r=r.substr(0,r.length-1)),ur.mkdir(r,e,0),0},doMknod:function(r,e,n){switch(61440&e){case 32768:case 8192:case 24576:case 4096:case 49152:break
default:return-28}return ur.mknod(r,e,n),0},doReadlink:function(r,e,n){if(n<=0)return-28
var t=ur.readlink(r),o=Math.min(n,S(t)),i=P[e+o]
return D(t,e,n+1),P[e+o]=i,o},doAccess:function(r,e){if(-8&e)return-28
var n
if(!(n=ur.lookupPath(r,{follow:!0}).node))return-44
var t=""
return 4&e&&(t+="r"),2&e&&(t+="w"),1&e&&(t+="x"),t&&ur.nodePermissions(n,t)?-2:0},doDup:function(r,e,n){var t=ur.getStream(n)
return t&&ur.close(t),ur.open(r,e,0,n,n).fd},doReadv:function(r,e,n,t){for(var o=0,i=0;i<n;i++){var a=M[e+8*i>>2],s=M[e+(8*i+4)>>2],u=ur.read(r,P,a,s,t)
if(u<0)return-1
if(o+=u,u<s)break}return o},doWritev:function(r,e,n,t){for(var o=0,i=0;i<n;i++){var a=M[e+8*i>>2],s=M[e+(8*i+4)>>2],u=ur.write(r,P,a,s,t)
if(u<0)return-1
o+=u}return o},varargs:void 0,get:function(){return cr.varargs+=4,M[cr.varargs-4>>2]},getStr:function(r){return k(r)},getStreamFromFD:function(r){var e=ur.getStream(r)
if(!e)throw new ur.ErrnoError(8)
return e},get64:function(r,e){return r}}
var lr=0
var fr={}
function dr(){if(!dr.strings){var r={USER:"web_user",LOGNAME:"web_user",PATH:"/",PWD:"/",HOME:"/home/web_user",LANG:("object"==typeof navigator&&navigator.languages&&navigator.languages[0]||"C").replace("-","_")+".UTF-8",_:i||"./this.program"}
for(var e in fr)r[e]=fr[e]
var n=[]
for(var e in r)n.push(e+"="+r[e])
dr.strings=n}return dr.strings}function mr(r,e){mr.array||(mr.array=[])
var n,t=mr.array
for(t.length=0;n=A[r++];)100===n||102===n?(e=e+7&-8,t.push(R[e>>3]),e+=8):(e=e+3&-4,t.push(M[e>>2]),e+=4)
return t}var pr=function(r,e,n,t){r||(r=this),this.parent=r,this.mount=r.mount,this.mounted=null,this.id=ur.nextInode++,this.name=e,this.mode=n,this.node_ops={},this.stream_ops={},this.rdev=t}
function hr(r,e,n){var t=n>0?n:S(r)+1,o=new Array(t),i=b(r,o,0,o.length)
return e&&(o.length=i),o}Object.defineProperties(pr.prototype,{read:{get:function(){return 365==(365&this.mode)},set:function(r){r?this.mode|=365:this.mode&=-366}},write:{get:function(){return 146==(146&this.mode)},set:function(r){r?this.mode|=146:this.mode&=-147}},isFolder:{get:function(){return ur.isDir(this.mode)}},isDevice:{get:function(){return ur.isChrdev(this.mode)}}}),ur.FSNode=pr,ur.staticInit()
var vr={a:function(r,e,n,t){G("Assertion failed: "+k(r)+", at: "+[e?k(e):"unknown filename",n,t?k(t):"unknown function"])},I:function(r,e){return function(r,e){var n
if(0===r)n=Date.now()
else{if(1!==r&&4!==r)return tr(28),-1
n=rr()}return M[e>>2]=n/1e3|0,M[e+4>>2]=n%1e3*1e3*1e3|0,0}(r,e)},F:function(r,e){return tr(63),-1},E:function(r,e){try{return r=cr.getStr(r),cr.doAccess(r,e)}catch(r){return void 0!==ur&&r instanceof ur.ErrnoError||G(r),-r.errno}},t:function(r,e,n){cr.varargs=n
try{var t=cr.getStreamFromFD(r)
switch(e){case 0:return(o=cr.get())<0?-28:ur.open(t.path,t.flags,0,o).fd
case 1:case 2:return 0
case 3:return t.flags
case 4:var o=cr.get()
return t.flags|=o,0
case 12:o=cr.get()
return x[o+0>>1]=2,0
case 13:case 14:return 0
case 16:case 8:return-28
case 9:return tr(28),-1
default:return-28}}catch(r){return void 0!==ur&&r instanceof ur.ErrnoError||G(r),-r.errno}},H:function(r,e){try{var n=cr.getStreamFromFD(r)
return cr.doStat(ur.stat,n.path,e)}catch(r){return void 0!==ur&&r instanceof ur.ErrnoError||G(r),-r.errno}},D:function(){return 42},L:function(r,e,n){cr.varargs=n
try{var t=cr.getStreamFromFD(r)
switch(e){case 21509:case 21505:return t.tty?0:-59
case 21510:case 21511:case 21512:case 21506:case 21507:case 21508:return t.tty?0:-59
case 21519:if(!t.tty)return-59
var o=cr.get()
return M[o>>2]=0,0
case 21520:return t.tty?-28:-59
case 21531:o=cr.get()
return ur.ioctl(t,e,o)
case 21523:case 21524:return t.tty?0:-59
default:G("bad ioctl syscall "+e)}}catch(r){return void 0!==ur&&r instanceof ur.ErrnoError||G(r),-r.errno}},K:function(r,e,n,t,o,i){try{return function(r,e,n,t,o,i){var a
i<<=12
var s=!1
if(0!=(16&t)&&r%16384!=0)return-28
if(0!=(32&t)){if(!(a=Sr(16384,e)))return-48
Dr(a,0,e),s=!0}else{var u=ur.getStream(o)
if(!u)return-8
var c=ur.mmap(u,A,r,e,i,n,t)
a=c.ptr,s=c.allocated}return cr.mappings[a]={malloc:a,len:e,allocated:s,fd:o,prot:n,flags:t,offset:i},a}(r,e,n,t,o,i)}catch(r){return void 0!==ur&&r instanceof ur.ErrnoError||G(r),-r.errno}},J:function(r,e){try{return function(r,e){if(-1==(0|r)||0===e)return-28
var n=cr.mappings[r]
if(!n)return 0
if(e===n.len){var t=ur.getStream(n.fd)
2&n.prot&&cr.doMsync(r,t,e,n.flags,n.offset),ur.munmap(t),cr.mappings[r]=null,n.allocated&&_r(n.malloc)}return 0}(r,e)}catch(r){return void 0!==ur&&r instanceof ur.ErrnoError||G(r),-r.errno}},p:function(r,e,n){cr.varargs=n
try{var t=cr.getStr(r),o=cr.get()
return ur.open(t,e,o).fd}catch(r){return void 0!==ur&&r instanceof ur.ErrnoError||G(r),-r.errno}},G:function(r,e){try{return r=cr.getStr(r),cr.doStat(ur.stat,r,e)}catch(r){return void 0!==ur&&r instanceof ur.ErrnoError||G(r),-r.errno}},M:function(r){try{return r=cr.getStr(r),ur.unlink(r),0}catch(r){return void 0!==ur&&r instanceof ur.ErrnoError||G(r),-r.errno}},P:function(){G()},w:function(r,e,n){var t=mr(e,n)
return er[r].apply(null,t)},e:function(r,e){!function(r,e){throw Fr(r,e||1),"longjmp"}(r,e)},y:function(r,e,n){A.copyWithin(r,e,e+n)},z:function(r){G("OOM")},B:function(r,e){var n=0
return dr().forEach((function(t,o){var i=e+n
M[r+4*o>>2]=i,function(r,e,n){for(var t=0;t<r.length;++t)P[e++>>0]=r.charCodeAt(t)
n||(P[e>>0]=0)}(t,i),n+=t.length+1})),0},C:function(r,e){var n=dr()
M[r>>2]=n.length
var t=0
return n.forEach((function(r){t+=r.length+1})),M[e>>2]=t,0},k:function(e){!function(e,n){if(n&&m&&0===e)return
m||(g=!0,r.onExit&&r.onExit(e))
a(e,new Hr(e))}(e)},m:function(r){try{var e=cr.getStreamFromFD(r)
return ur.close(e),0}catch(r){return void 0!==ur&&r instanceof ur.ErrnoError||G(r),r.errno}},A:function(r,e){try{var n=cr.getStreamFromFD(r),t=n.tty?2:ur.isDir(n.mode)?3:ur.isLink(n.mode)?7:4
return P[e>>0]=t,0}catch(r){return void 0!==ur&&r instanceof ur.ErrnoError||G(r),r.errno}},N:function(r,e,n,t){try{var o=cr.getStreamFromFD(r),i=cr.doReadv(o,e,n)
return M[t>>2]=i,0}catch(r){return void 0!==ur&&r instanceof ur.ErrnoError||G(r),r.errno}},x:function(r,e,n,t,o){try{var i=cr.getStreamFromFD(r),a=4294967296*n+(e>>>0)
return a<=-9007199254740992||a>=9007199254740992?-61:(ur.llseek(i,a,t),$=[i.position>>>0,(Z=i.position,+L(Z)>=1?Z>0?(0|H(+U(Z/4294967296),4294967295))>>>0:~~+I((Z-+(~~Z>>>0))/4294967296)>>>0:0)],M[o>>2]=$[0],M[o+4>>2]=$[1],i.getdents&&0===a&&0===t&&(i.getdents=null),0)}catch(r){return void 0!==ur&&r instanceof ur.ErrnoError||G(r),r.errno}},q:function(r,e,n,t){try{var o=cr.getStreamFromFD(r),i=cr.doWritev(o,e,n)
return M[t>>2]=i,0}catch(r){return void 0!==ur&&r instanceof ur.ErrnoError||G(r),r.errno}},b:function(){return 0|h},Q:function(r){var e=Lr()
try{return Or(r)}catch(r){if(Ur(e),r!==r+0&&"longjmp"!==r)throw r
Fr(1,0)}},v:function(r){var e=Lr()
try{return Cr(r)}catch(r){if(Ur(e),r!==r+0&&"longjmp"!==r)throw r
Fr(1,0)}},j:function(r,e){var n=Lr()
try{return Tr(r,e)}catch(r){if(Ur(n),r!==r+0&&"longjmp"!==r)throw r
Fr(1,0)}},h:function(r,e,n){var t=Lr()
try{return jr(r,e,n)}catch(r){if(Ur(t),r!==r+0&&"longjmp"!==r)throw r
Fr(1,0)}},f:function(r,e,n,t){var o=Lr()
try{return Br(r,e,n,t)}catch(r){if(Ur(o),r!==r+0&&"longjmp"!==r)throw r
Fr(1,0)}},O:function(r,e,n,t,o,i,a){var s=Lr()
try{return Nr(r,e,n,t,o,i,a)}catch(r){if(Ur(s),r!==r+0&&"longjmp"!==r)throw r
Fr(1,0)}},i:function(r){var e=Lr()
try{Pr(r)}catch(r){if(Ur(e),r!==r+0&&"longjmp"!==r)throw r
Fr(1,0)}},g:function(r,e){var n=Lr()
try{Ar(r,e)}catch(r){if(Ur(n),r!==r+0&&"longjmp"!==r)throw r
Fr(1,0)}},n:function(r,e,n){var t=Lr()
try{xr(r,e,n)}catch(r){if(Ur(t),r!==r+0&&"longjmp"!==r)throw r
Fr(1,0)}},s:function(r,e,n,t){var o=Lr()
try{Mr(r,e,n,t)}catch(r){if(Ur(o),r!==r+0&&"longjmp"!==r)throw r
Fr(1,0)}},r:function(r,e,n,t,o){var i=Lr()
try{Rr(r,e,n,t,o)}catch(r){if(Ur(i),r!==r+0&&"longjmp"!==r)throw r
Fr(1,0)}},o:function(r,e,n,t,o,i){var a=Lr()
try{zr(r,e,n,t,o,i)}catch(r){if(Ur(a),r!==r+0&&"longjmp"!==r)throw r
Fr(1,0)}},memory:p,l:function r(e,n,t,o){n|=0,t|=0,o|=0
var i=0
for(lr=lr+1|0,M[(e|=0)>>2]=lr;(0|i)<(0|o);){if(0==(0|M[t+(i<<3)>>2]))return M[t+(i<<3)>>2]=lr,M[t+(4+(i<<3))>>2]=n,M[t+(8+(i<<3))>>2]=0,v(0|o),0|t
i=i+1|0}return t=0|r(0|e,0|n,0|(t=0|kr(0|t,8*((o=2*o|0)+1|0)|0)),0|o),v(0|o),0|t},c:function(r){v(0|r)},table:w,d:function(r,e,n){r|=0,e|=0,n|=0
for(var t=0,o=0;(0|t)<(0|n)&&0!=(0|(o=0|M[e+(t<<3)>>2]));){if((0|o)==(0|r))return 0|M[e+(4+(t<<3))>>2]
t=t+1|0}return 0},u:function(r){return 0!==r&&Dr(r,0,16),0}},wr=function(){var e={a:vr}
function n(e,n){var t=e.exports
r.asm=t,X()}function i(r){n(r.instance)}function a(r){return(d||!t&&!o||"function"!=typeof fetch?new Promise((function(r,e){r(Q())})):fetch(J,{credentials:"same-origin"}).then((function(r){if(!r.ok)throw"failed to load wasm binary file at '"+J+"'"
return r.arrayBuffer()})).catch((function(){return Q()}))).then((function(r){return WebAssembly.instantiate(r,e)})).then(r,(function(r){f("failed to asynchronously prepare wasm: "+r),G(r)}))}if(V(),r.instantiateWasm)try{return r.instantiateWasm(e,n)}catch(r){return f("Module.instantiateWasm callback failed with error: "+r),!1}return function(){if(d||"function"!=typeof WebAssembly.instantiateStreaming||K(J)||"function"!=typeof fetch)return a(i)
fetch(J,{credentials:"same-origin"}).then((function(r){return WebAssembly.instantiateStreaming(r,e).then(i,(function(r){f("wasm streaming compile failed: "+r),f("falling back to ArrayBuffer instantiation"),a(i)}))}))}(),{}}()
r.asm=wr
var gr,yr=r.___wasm_call_ctors=function(){return(yr=r.___wasm_call_ctors=r.asm.R).apply(null,arguments)},Er=(r._vizLastErrorMessage=function(){return(r._vizLastErrorMessage=r.asm.S).apply(null,arguments)},r._vizCreateFile=function(){return(r._vizCreateFile=r.asm.T).apply(null,arguments)},r._vizSetY_invert=function(){return(r._vizSetY_invert=r.asm.U).apply(null,arguments)},r._vizSetNop=function(){return(r._vizSetNop=r.asm.V).apply(null,arguments)},r._vizRenderFromString=function(){return(r._vizRenderFromString=r.asm.W).apply(null,arguments)},r._malloc=function(){return(Er=r._malloc=r.asm.X).apply(null,arguments)}),_r=r._free=function(){return(_r=r._free=r.asm.Y).apply(null,arguments)},kr=(r._dtopen=function(){return(r._dtopen=r.asm.Z).apply(null,arguments)},r._realloc=function(){return(kr=r._realloc=r.asm._).apply(null,arguments)}),br=r.___errno_location=function(){return(br=r.___errno_location=r.asm.$).apply(null,arguments)},Dr=r._memset=function(){return(Dr=r._memset=r.asm.aa).apply(null,arguments)},Sr=(r._dtextract=function(){return(r._dtextract=r.asm.ba).apply(null,arguments)},r._dtdisc=function(){return(r._dtdisc=r.asm.ca).apply(null,arguments)},r._memalign=function(){return(Sr=r._memalign=r.asm.da).apply(null,arguments)}),Fr=r._setThrew=function(){return(Fr=r._setThrew=r.asm.ea).apply(null,arguments)},Pr=r.dynCall_v=function(){return(Pr=r.dynCall_v=r.asm.fa).apply(null,arguments)},Ar=r.dynCall_vi=function(){return(Ar=r.dynCall_vi=r.asm.ga).apply(null,arguments)},xr=r.dynCall_vii=function(){return(xr=r.dynCall_vii=r.asm.ha).apply(null,arguments)},Mr=r.dynCall_viii=function(){return(Mr=r.dynCall_viii=r.asm.ia).apply(null,arguments)},Rr=r.dynCall_viiii=function(){return(Rr=r.dynCall_viiii=r.asm.ja).apply(null,arguments)},zr=r.dynCall_viiiii=function(){return(zr=r.dynCall_viiiii=r.asm.ka).apply(null,arguments)},Cr=r.dynCall_i=function(){return(Cr=r.dynCall_i=r.asm.la).apply(null,arguments)},Tr=r.dynCall_ii=function(){return(Tr=r.dynCall_ii=r.asm.ma).apply(null,arguments)},jr=r.dynCall_iii=function(){return(jr=r.dynCall_iii=r.asm.na).apply(null,arguments)},Br=r.dynCall_iiii=function(){return(Br=r.dynCall_iiii=r.asm.oa).apply(null,arguments)},Nr=r.dynCall_iiiiiii=function(){return(Nr=r.dynCall_iiiiiii=r.asm.pa).apply(null,arguments)},Or=r.dynCall_d=function(){return(Or=r.dynCall_d=r.asm.qa).apply(null,arguments)},Lr=r.stackSave=function(){return(Lr=r.stackSave=r.asm.ra).apply(null,arguments)},Ir=r.stackAlloc=function(){return(Ir=r.stackAlloc=r.asm.sa).apply(null,arguments)},Ur=r.stackRestore=function(){return(Ur=r.stackRestore=r.asm.ta).apply(null,arguments)}
function Hr(r){this.name="ExitStatus",this.message="Program terminated with exit("+r+")",this.status=r}function Wr(e){function n(){gr||(gr=!0,r.calledRun=!0,g||(r.noFSInit||ur.init.initialized||ur.init(),ar.init(),T(B),ur.ignorePermissions=!1,T(N),r.onRuntimeInitialized&&r.onRuntimeInitialized(),function(){if(r.postRun)for("function"==typeof r.postRun&&(r.postRun=[r.postRun]);r.postRun.length;)e=r.postRun.shift(),O.unshift(e)
var e
T(O)}()))}W>0||(!function(){if(r.preRun)for("function"==typeof r.preRun&&(r.preRun=[r.preRun]);r.preRun.length;)e=r.preRun.shift(),j.unshift(e)
var e
T(j)}(),W>0||(r.setStatus?(r.setStatus("Running..."),setTimeout((function(){setTimeout((function(){r.setStatus("")}),1),n()}),1)):n()))}if(r.asm=wr,r.ccall=function(r,e,n,t,o){var i={string:function(r){var e=0
if(null!=r&&0!==r){var n=1+(r.length<<2)
D(r,e=Ir(n),n)}return e},array:function(r){var e=Ir(r.length)
return function(r,e){P.set(r,e)}(r,e),e}},a=y(r),s=[],u=0
if(t)for(var c=0;c<t.length;c++){var l=i[n[c]]
l?(0===u&&(u=Lr()),s[c]=l(t[c])):s[c]=t[c]}var f=a.apply(null,s)
return f=function(r){return"string"===e?k(r):"boolean"===e?Boolean(r):r}(f),0!==u&&Ur(u),f},r.UTF8ToString=k,q=function r(){gr||Wr(),gr||(q=r)},r.run=Wr,r.preInit)for("function"==typeof r.preInit&&(r.preInit=[r.preInit]);r.preInit.length>0;)r.preInit.pop()()
return m=!0,Wr(),r})}("undefined"!=typeof self?self:window)
const e=new class{constructor(r,e){let n=void 0,t=!1,o=new Promise((e,o)=>{try{n=r(),n.onRuntimeInitialized=()=>{t=!0,e()}}catch(r){o(r)}})
this.render=async(r,i)=>(t||await o,e(n,r,i))}}(Viz.Module,Viz.render),n={format:"svg",engine:"dot",files:[],images:[],yInvert:!1,nop:0}
r.viz=function(r,t){const o=t?Object.assign({},n,t):n
return e.render(r,o)}}()
