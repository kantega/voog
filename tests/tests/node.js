(async function() {
    var send = await createTest("Single node");
    send({
        "setNodes": [{'id': 0}]
    });
})();
