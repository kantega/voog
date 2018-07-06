(async function() {
    var send = await createTest("Force distance");
    send({
        'setNodes': [
            {'id': 0, 'x': 0, 'y': 0},
            {'id': 1, 'x': 1, 'y': 0},
            {'id': 2, 'x': 5, 'y': -5},
            {'id': 3, 'x': -5, 'y': 5},
        ],
        'layout': 'manual.forced',
        'repulsion': 1000000,
    });
})();
