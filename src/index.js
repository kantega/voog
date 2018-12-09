import {Voog} from "./voog"

const voog = Voog.init('main');

voog.send(
    {
        setNodes: [
            {id: 0, name: "Hello"},
            {id: 1, name: "World!"}
        ],
        setEdges: [
            {from: 0, to: 1}
        ],
    }
);

setInterval(function () {
    voog.send({
        'addMovement': [{'from': 0, 'to': 1, 'duration': 1, 'icon': 'dot'}]
    });
}, 500);
