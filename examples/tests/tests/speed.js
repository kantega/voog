(async function() {
    var send = await createTest("Speed");
    send({
        "setEdges": [{'from': 0, 'to': 1, 'speed': 30}]
    });
})();
