(async function() {
    var send = await createTest("Children and parents");
    send({
        "setEdges": [{'from': 1, 'to': 0}, {'from': 2, 'to': 0}, {'from': 0, 'to': 3}, {'from': 0, 'to': 4}]
    });
})();
