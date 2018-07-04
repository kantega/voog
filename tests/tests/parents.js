(async function() {
    var send = await createTest("Parents");
    send({
        "setEdges": [{'from': 1, 'to': 0}, {'from': 2, 'to': 0}]
    });
})();
