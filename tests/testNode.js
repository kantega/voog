async function testNode() {
    var send = await createTest("Single node");
    send({
        "setNodes": [{'id': 0}]
    });
}

testNode();
