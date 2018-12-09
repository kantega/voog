(async function() {
    var send = await createTest("Children");
    send({
        "setEdges": [{'from': 0, 'to': 1}, {'from': 0, 'to': 2}]
    });
})();
