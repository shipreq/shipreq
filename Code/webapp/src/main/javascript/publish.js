function renderAll() {
    var data_dot_attr = 'dot'
    $('tr.flowgraph').each(function(i,p){
        var id = "fg-"+i
        $(p).attrAdd('id', id)
        var dot = $(p).find('[data-'+data_dot_attr+']').data(data_dot_attr)
        var tgtSel = '#'+id+' td'
        VizWorker.postMessage({tgt:tgtSel, dot:dot})
    })
}

$(document).ready(function(){
    setupViz()
    renderAll()
})
