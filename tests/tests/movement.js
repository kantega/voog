(async function() {
    var send = await createTest("Movement");
    send({
        'setEdges': [{'from': 0, 'to': 1}, {'from': 1, 'to': 2}, {'from': 2, 'to': 0}]
    });
    setInterval(function() {
        send({
            'addMovement': [{'from': 0, 'to': 1, 'duration': 2, 'icon': 'plane'}]
        });
    }, 600);
    setInterval(function() {
        send({
            'addMovement': [{'from': 2, 'to': 0, 'duration': 0.5, 'icon': 'dot'}]
        });
    }, 200);
    setInterval(function() {
        send({
            'addMovement': [{'from': 1, 'to': 2, 'duration': 1, 'icon': 'message'}]
        });
    }, 800);
})();
