(async function() {
    var send = await createTest("Remove edges");
    setInterval(function() {
        send({
            "setEdges": [{'from': 0, 'to': 1}],
            "center": true
        });
        setTimeout(function() {
            send({
                "removeEdges": [[0, 1]],
                "center": true
            });
        }, 500);
        setTimeout(function() {
            send({
                "removeNodes": [0, 1],
                "center": true
            });
        }, 1000);
    }, 1500);
})();
