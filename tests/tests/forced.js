(async function() {
    var send = await createTest("Forced");
    send({
        'setEdges': edges([
            [-1, 0], [0, 1], [0, 2], [1, 3], [1, 4], [2, 5], [5, 6], [5, 7], [5, 8], [5, 9]
        ]),
        'layout': 'forced'
    });
})();
