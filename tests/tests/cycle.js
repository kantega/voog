(async function() {
    var send = await createTest("Cycle");
    send({
        "setEdges": [{'from': 0, 'to': 1}, {'from': 1, 'to': 2}, {'from': 2, 'to': 0}]
    });
})();
