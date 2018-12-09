(async function() {
    var send = await createTest("Manual");
    send({
        'setNodes': [
            {'id': 0, 'x': -10, 'y': 320, 'name': '-10, 320'},
            {'id': 1, 'x': 320, 'y': 400, 'name': '320, 400'},
            {'id': 2, 'x': 80, 'y': -80, 'name': '80, -80'}

        ],
        'layout': 'manual'
    });
})();
