importScripts("vendor/viz.js")

onmessage = function(ev) {
	var d = ev.data
	d.svg = Viz(d.dot,'svg')
	d.dot = undefined
	self.postMessage(d)
}
