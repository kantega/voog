(async function() {
    var send = await createTest("Stuck on left");
    send({
        "setEdges": edges([[0,1], [1,2], [2,3], [3,4], [3,5], [4,6], [4,7], [5,8], [5,9], [5,10], [0, -1], [-1, -2]])
    });
})();
