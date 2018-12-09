(async function() {
    var send = await createTest("Children left of root");
    send({
        "setEdges": [{'from': 0, 'to': 1}, {'from': 0, 'to': 2}, {'from': 0, 'to': 3},
            {'from': 0, 'to': 4}, {'from': 1, 'to': 5}, {'from': 1, 'to': 6}, {'from': 1, 'to': 7}]
    });
})();
