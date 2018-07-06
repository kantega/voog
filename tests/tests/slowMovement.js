(async function() {
    var send = await createTest("Slow movement");
    send({
        'setEdges': [{'from': 0, 'to': 1}],
        'setNodes': [{'id': 0, 'name': 'node 0'}, {'id': 1, 'name': 'node 1'}],
        'layout': 'forced',
        'attraction': 0,
        'repulsion': 40000,
    });
})();
