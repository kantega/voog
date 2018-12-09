(async function() {
    var send = await createTest("Remove nodes");
    setInterval(function() {
        send({
            "setNodes": [{'id': 0}]
        });
        setTimeout(function() {
            send({
                "removeNodes": [0]
            });
        }, 500);
    }, 1000);
})();
