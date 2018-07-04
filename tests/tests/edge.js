(async function() {
    var send = await createTest("Single edge");
    send({
        "setEdges": [{'from': 0, 'to': 1}]
    });
})();
