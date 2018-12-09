(async function() {
    var send = await createTest("Delayed Name");
    function run() {
        send({
            "setNodes": [{'id': 0}],
            "clear": true,
        });
        setTimeout(function () {
            send({
                "setNodes": [{'id': 0, 'name': 'name'}]
            });
        }, 1000);
    }
    run();
    setInterval(run, 3000);
})();
