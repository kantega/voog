var treeNode = document.getElementById('tree');
var treeApp = Elm.Main.embed(treeNode, {'webSocket': null});

function genTree(layers, minNodes, maxNodes, nextId, nodes, edges) {
    var thisId = nextId++;
    nodes.push(thisId);
    if (layers > 0) {
        for (var i = 0; i < minNodes + Math.floor(Math.random() * (maxNodes - minNodes)); i++) {
            edges.push([thisId, nextId]);
            var result = genTree(layers - 1, minNodes, maxNodes, nextId, nodes, edges);
            nextId = result[0];
            nodes = result[1];
            edges = result[2];
        }
    }
    return [nextId, nodes, edges];
}

function createGraph(minNodes, maxNodes) {
    treeApp.ports.input.send(JSON.stringify({
        "removeNodes": [...Array(maxNodes).keys()]
    }));

    var nodes = [];
    while (nodes.length < minNodes || nodes.length > maxNodes) {
        result = genTree(3, 0, 4, 0, [], []);
        nodes = result[1];
        edges = result[2];
    }

    sendEdges = [];
    sendNodes = [];

    for (var i = 0; i < edges.length; i++) {
        sendEdges.push({"from": edges[i][0], "to": edges[i][1]})
    }
    for (var i = 0; i < nodes.length; i++) {
        sendNodes.push({"id": nodes[i]})
    }

    treeApp.ports.input.send(JSON.stringify({
        "name": "tree",
        "size": [treeNode.offsetWidth, treeNode.offsetHeight],
        "position": [treeNode.offsetLeft, treeNode.offsetTop],
        "setNodes": sendNodes,
        "setEdges": sendEdges
    }));
}

setTimeout(function () {createGraph(10, 100);}, 10);
setInterval(function () {createGraph(10, 100);}, 2000);
