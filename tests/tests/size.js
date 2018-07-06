(async function() {
    var send = await createTest("Node size");
    send({
        "setNodes": [
            {'id': 1, 'shape': 'rect', 'height': 100},
            {'id': 2, 'shape': 'rect', 'width': 100},
            {'id': 3, 'width': 100},
            {'id': 4, 'height': 100}
        ],
        "setEdges": [{'from': 0, 'to': 1}, {'from': 0, 'to': 2}, {'from': 0, 'to': 3}, {'from': 0, 'to': 4}],
    });
})();
