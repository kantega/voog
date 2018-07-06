(async function() {
    var send = await createTest("Invalid Input");

    function run() {
        send({
            'setNodes': [{'id': 0}]
        });
        setTimeout(function() {
            send({
                'setNodes': [{'from': 1}]
            });
        }, 1000);
    }

    run();
    setInterval(function() {
        run()
    }, 3000);
})();
