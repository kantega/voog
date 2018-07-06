(async function() {
    var send = await createTest("Images");
    send({
        "setNodes": [
            {'id': 1, 'shape': 'rect', 'height': 100, 'name': 'A'},
            {'id': 2, 'shape': 'rect', 'width': 100, 'name': 'B'},
            {'id': 3, 'width': 100, 'name': 'C'},
            {'id': 4, 'height': 100, 'name': 'D'},
            {'id': 5, 'shape': 'rect', 'height': 100, 'name': 'E', "image": "https://images.pexels.com/photos/66997/pexels-photo-66997.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=32&w=32"},
            {'id': 6, 'shape': 'rect', 'width': 100, 'name': 'F', "image": "https://images.pexels.com/photos/66997/pexels-photo-66997.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=32&w=32"},
            {'id': 7, 'width': 100, 'name': 'G', "image": "https://images.pexels.com/photos/66997/pexels-photo-66997.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=32&w=32"},
            {'id': 8, 'height': 100, 'name': 'H', "image": "https://images.pexels.com/photos/66997/pexels-photo-66997.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=32&w=32"}
        ],
        "setEdges": [
            {'from': 0, 'to': 1},
            {'from': 0, 'to': 2},
            {'from': 0, 'to': 3},
            {'from': 0, 'to': 4},
            {'from': 1, 'to': 7},
            {'from': 2, 'to': 8},
            {'from': 3, 'to': 5},
            {'from': 4, 'to': 6}
        ]
    });
})();
