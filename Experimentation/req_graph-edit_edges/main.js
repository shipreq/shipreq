let e = null

const svg = document.querySelector("svg")
const fr54 = document.getElementById("FR-54")
const fr79 = document.getElementById("FR-79")

const edges = document.querySelectorAll("g.edge")
console.log("Edges: ", edges.length)

const nodes = document.querySelectorAll("g.node")
console.log("Nodes: ", nodes.length)

// ====================================================================================================================================
// Selecting an edge

function onEdgeClick(ev) {
  // console.log("Click event: ", ev)
  e = ev.currentTarget.parentElement
  // console.log("Clicked: ", e)
  if (e.classList.contains('selected_edge')) {
    e.classList.remove('selected_edge')
  } else {
    for (i of document.querySelectorAll('.selected_edge'))
      i.classList.remove('selected_edge')
    e.classList.add('selected_edge')
  }
}

for (const e of edges) {
  // e.onclick = onEdgeClick
  // e.setAttribute("stroke-width", 5)

  const e2 = e.cloneNode(true)
  e2.classList.remove("edge")
  e2.classList.add("edge2")
  e2.setAttribute("stroke-width", 16)
  e.append(e2)

  e2.onclick = onEdgeClick

  // const parent = e.parentElement
  // parent.replaceChild(e2, e)
  // e2.append(e)
}

// console.log("Selected: ", document.querySelectorAll('.selected_edge').length)

// ====================================================================================================================================
// Creating a new edge

// Draggable.create("g.node")

let dragSrc = null
let dragTgt = null

const dragArrow1 = document.getElementById("dragArrow1")
const point = svg.createSVGPoint()

// https://stackoverflow.com/a/50396546/1846272
function getCTM() {
  var owner = svg,
      height = owner.height.baseVal.value,
      width = owner.width.baseVal.value,
      viewBoxRect = owner.viewBox.baseVal,
      vHeight = viewBoxRect.height,
      vWidth = viewBoxRect.width;
  if(!vWidth || !vHeight){
      return svg.getCTM();
  }
  var sH = height/vHeight,
      sW = width/vWidth,
      matrix = owner.createSVGMatrix();
  matrix.a = sW;
  matrix.d = sH
  var realCTM = svg.getCTM().multiply(matrix.inverse());
  realCTM.e = realCTM.e/sW + viewBoxRect.x;
  realCTM.f = realCTM.f/sH + viewBoxRect.y;
  return realCTM;
}

function getNodeMidpoint(node) {
  // const ctm = node.getCTM()
  const ctm = getCTM()
  const bb  = node.getBBox()
  point.x   = bb.x + bb.width/2
  point.y   = bb.y + bb.height/2
  return point.matrixTransform(ctm)
}

function calcAngleDegrees(x, y) {
  return Math.atan2(y, x) * 180 / Math.PI
}

function setDragSrc(d) {
  if (d !== dragSrc) {
    if (dragSrc) dragSrc.classList.remove("dragSrc")
    dragSrc = d
    if (dragSrc) dragSrc.classList.add("dragSrc")
  }
}

function setDragTgt(d) {
  if (d !== dragTgt) {
    if (dragTgt) dragTgt.classList.remove("dragTgt")
    dragTgt = d
    if (dragTgt) dragTgt.classList.add("dragTgt")
  }
}

function onNodeMouseDown(ev) {
  // console.log("onNodeMouseDown: ", ev)
  setDragSrc(ev.currentTarget)
  dragArrow1.setAttribute("d", "")
  svg.classList.add("dragging")
}

function onDragArrowMouseMove(ev) {
  ev.stopPropagation()
}

function onNodeMouseMove(ev) {
  // console.log("onNodeMouseMove: ", ev)
  if (dragSrc) {
    if (ev.currentTarget !== dragTgt) {
      setDragTgt(ev.currentTarget)

      const a = getNodeMidpoint(dragSrc)
      const b = getNodeMidpoint(dragTgt)

      const lx = b.x - a.x
      const ly = b.y - a.y
      const ll = Math.sqrt(lx**2 + ly**2)
      const r = 6
      const l = `M${a.x},${a.y} h${ll} l-${r},-${r} l${r},${r} l-${r},${r}`
      const deg = calcAngleDegrees(lx, ly)
      const t = `rotate(${deg},${a.x},${a.y})`
      dragArrow1.setAttribute("d", l)
      dragArrow1.setAttribute("transform", t)
      // console.log(t)
    }
  }
  ev.stopPropagation()
}

function onSvgMouseMove(ev) {
  // console.log(`${ev.clientX},${ev.clientY}`)
  // console.log("onSvgMouseMove: ", ev)
  if (dragSrc) {
    setDragTgt(null)
    dragArrow1.setAttribute("d", "")
  }
}

// function onNodeMouseUp(ev) {
//   // console.log("onNodeMouseUp: ", ev)
//   const dragTgt = ev.currentTarget
//   console.log(`New edge: ${dragSrc.id} -> ${dragTgt.id}`)
//   // This will then bubble into onSvgMouseUp
// }

function onSvgMouseUp(ev) {
  // console.log("onSvgMouseUp: ", ev)
  if (dragSrc) {
    svg.classList.remove("dragging")
    if (dragTgt) {
      console.log(`New edge: ${dragSrc.id} -> ${dragTgt.id}`)
      setDragTgt(null)
    }
    setDragSrc(null)
    dragArrow1.setAttribute("d", "")
  }
}

for (const n of nodes) {
  n.onmousedown = onNodeMouseDown
  n.onmousemove = onNodeMouseMove
}

svg.onmousemove = onSvgMouseMove
svg.onmouseup   = onSvgMouseUp

dragArrow1.onmousemove = onDragArrowMouseMove