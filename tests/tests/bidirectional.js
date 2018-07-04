(async function() {
    var send = await createTest("Bidirectional edge");
    send({
        "setEdges": [{'from': 0, 'to': 1}, {'from': 1, 'to': 0}]
    });
})();
