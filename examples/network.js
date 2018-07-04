var networkNode = document.getElementById('network');
var networkApp = Elm.Main.embed(networkNode, {'webSocket': null, 'disableWindowResize': false});

var server_img = "";
var router_img = "";
var laptop_img = "";
var mobile_img = "";
var desktop_img = "";


class Agent {
    constructor(lower, upper) {
        this.lower = lower;
        this.upper = upper;
        this.speed = lower + Math.random() * (upper - lower);
        this.direction = 1;
    }

    update() {
        if (Math.random() < 0.15) {
            this.direction *= -1;
        }
        this.speed += this.direction * Math.random() * (this.upper - this.lower) / 10;
        if (this.speed >= this.upper) {
            this.speed = this.upper;
            this.direction = -1;
        } else if (this.speed <= this.lower) {
            this.speed = this.lower;
            this.direction = 1;
        }
    }

    getSpeed() {
        return this.speed;
    }
}

class DoubleAgent {
    constructor(lower = 0, upper= 0) {
        this.up = new Agent(lower, upper);
        this.down = new Agent(lower, upper);
    }

    update() {
        this.up.update();
        this.down.update();
    }

    getUpSpeed() {
        return this.up.getSpeed();
    }

    getDownSpeed() {
        return this.down.getSpeed();
    }

}

class Node {
    constructor(children = []) {
        this.children = children;
    }

    update() {
        for (var i = 0; i < this.children.length; i++) {
            this.children[i].update();
        }
    }

    add(element) {
        this.children.push(element);
    }

    remove(element) {
        var i = this.children.indexOf(5);
        if (i >= 0) {
            this.children.splice(i, 1);
        }
    }

    getUpSpeed() {
        return this.children.reduce((a, b) => a + b.getUpSpeed(), 0);
    }

    getDownSpeed() {
        return this.children.reduce((a, b) => a + b.getDownSpeed(), 0);
    }
}

var agent4 = new DoubleAgent(0, 30);
var agent5 = new DoubleAgent(0, 12);
var agent6 = new DoubleAgent(0, 12);
var agent7 = new DoubleAgent(0, 30);
var agent8 = new DoubleAgent(0, 30);
var agent9 = new DoubleAgent(0, 30);
var agent10 = new DoubleAgent(0, 30);
var agent11 = new DoubleAgent(0, 12);
var agent12 = new DoubleAgent(0, 30);
var agent13 = new DoubleAgent(0, 30);
var agent910 = new DoubleAgent(0, 60);
var node1 = new Node([agent4, agent5, agent6]);
var node2 = new Node([agent7, agent8, agent9]);
var node3 = new Node([agent10, agent11]);
var node0 = new Node([node1, node2, node3, agent12, agent13]);

function run() {
    node0.update();
    agent910.update();
    var nodes = [
        {
            "id": 0,
            "name": "Server",
            "image": server_img,
            "classes": ["error"],
            "shape": "rect",
            "category": "a",
            "size": 60,
            "info": [["ip", "193.69.210.242"], ["type", "server"], ["model", "DSE-4560K"]]
        },
        {"id": 1, "name": "Entrance", "image": router_img, "category": "b", "info": [["ip", "192.168.0.1"]]},
        {"id": 2, "name": "Office", "image": router_img, "category": "b", "info": [["ip", "192.168.0.2"]]},
        {"id": 3, "name": "Cafeteria", "image": router_img, "category": "b", "info": [["ip", "192.168.0.3"]]},
        {"id": 4, "name": "Eula", "image": laptop_img, "category": "c", "info": [["ip", "192.168.0.4"]]},
        {"id": 5, "name": "Elicia", "image": mobile_img, "category": "c", "info": [["ip", "192.168.0.5"]]},
        {"id": 6, "name": "Theo", "image": mobile_img, "category": "c", "info": [["ip", "192.168.0.6"]]},
        {"id": 7, "name": "May", "image": laptop_img, "category": "c", "info": [["ip", "192.168.0.7"]]},
        {
            "id": 8,
            "name": "Kyle",
            "image": laptop_img,
            "classes": ["highlight"],
            "category": "c",
            "color": "#235893",
            "info": [["ip", "192.168.0.8"]]
        },
        {"id": 9, "name": "Carrie", "image": laptop_img, "category": "c", "info": [["ip", "192.168.0.9"]]},
        {"id": 10, "name": "Denny", "image": laptop_img, "category": "c", "info": [["ip", "192.168.0.10"]]},
        {"id": 11, "name": "Daria", "image": mobile_img, "category": "c", "info": [["ip", "192.168.0.11"]]},
        {"id": 12, "name": "Kendra", "image": desktop_img, "info": [["ip", "192.168.0.12"]]},
        {"id": 13, "name": "Stew", "image": desktop_img, "info": [["ip", "192.168.0.13"]]}
    ];

    var edges = [
        {
            "from": 0,
            "to": 1,
            "speed": node1.getDownSpeed() * 5,
            "label": String(Math.round(node1.getDownSpeed())),
            "info": [["speed", String(Math.round(node1.getDownSpeed()))]]
        },
        {
            "from": 0,
            "to": 2,
            "speed": node2.getDownSpeed() * 5,
            "label": String(Math.round(node2.getDownSpeed())),
            "info": [["speed", String(Math.round(node2.getDownSpeed()))]]
        },
        {
            "from": 0,
            "to": 3,
            "speed": node3.getDownSpeed() * 5,
            "label": String(Math.round(node3.getDownSpeed())),
            "info": [["speed", String(Math.round(node3.getDownSpeed()))]]
        },
        {
            "from": 0,
            "to": 12,
            "speed": agent12.getDownSpeed() * 5,
            "label": String(Math.round(agent12.getDownSpeed())),
            "info": [["speed", String(Math.round(agent12.getDownSpeed()))]]
        },
        {
            "from": 0,
            "to": 13,
            "speed": agent13.getDownSpeed() * 5,
            "label": String(Math.round(agent13.getDownSpeed())),
            "info": [["speed", String(Math.round(agent13.getDownSpeed()))]]
        },
        {
            "from": 1,
            "to": 4,
            "speed": agent4.getDownSpeed() * 5,
            "label": String(Math.round(agent4.getDownSpeed())),
            "info": [["speed", String(Math.round(agent4.getDownSpeed()))]]
        },
        {
            "from": 1,
            "to": 5,
            "speed": agent5.getDownSpeed() * 5,
            "label": String(Math.round(agent5.getDownSpeed())),
            "info": [["speed", String(Math.round(agent5.getDownSpeed()))]]
        },
        {
            "from": 1,
            "to": 6,
            "speed": agent6.getDownSpeed() * 5,
            "label": String(Math.round(agent6.getDownSpeed())),
            "classes": ["error"],
            "info": [["speed", String(Math.round(agent6.getDownSpeed()))]]
        },
        {
            "from": 2,
            "to": 7,
            "speed": agent7.getDownSpeed() * 5,
            "label": String(Math.round(agent7.getDownSpeed())),
            "classes": ["inverted"],
            "info": [["speed", String(Math.round(agent7.getDownSpeed()))]]
        },
        {
            "from": 2,
            "to": 8,
            "speed": agent8.getDownSpeed() * 5,
            "label": String(Math.round(agent8.getDownSpeed())),
            "info": [["speed", String(Math.round(agent8.getDownSpeed()))]]
        },
        {
            "from": 2,
            "to": 9,
            "speed": agent9.getDownSpeed() * 5,
            "label": String(Math.round(agent9.getDownSpeed())),
            "info": [["speed", String(Math.round(agent9.getDownSpeed()))]]
        },
        {
            "from": 3,
            "to": 10,
            "speed": agent10.getDownSpeed() * 5,
            "label": String(Math.round(agent10.getDownSpeed())),
            "info": [["speed", String(Math.round(agent10.getDownSpeed()))]]
        },
        {
            "from": 3,
            "to": 11,
            "speed": agent11.getDownSpeed() * 5,
            "label": String(Math.round(agent11.getDownSpeed())),
            "info": [["speed", String(Math.round(agent11.getDownSpeed()))]]
        },
        {
            "from": 10,
            "to": 9,
            "speed": agent910.getDownSpeed() * 5,
            "label": String(Math.round(agent910.getDownSpeed())),
            "info": [["speed", String(Math.round(agent910.getDownSpeed()))]]
        },

        {
            "from": 1,
            "to": 0,
            "speed": node1.getUpSpeed() * 5,
            "label": String(Math.round(node1.getUpSpeed())),
            "info": [["speed", String(Math.round(node1.getUpSpeed()))]]
        },
        {
            "from": 2,
            "to": 0,
            "speed": node2.getUpSpeed() * 5,
            "label": String(Math.round(node2.getUpSpeed())),
            "info": [["speed", String(Math.round(node2.getUpSpeed()))]]
        },
        {
            "from": 3,
            "to": 0,
            "speed": node3.getUpSpeed() * 5,
            "label": String(Math.round(node3.getUpSpeed())),
            "info": [["speed", String(Math.round(node3.getUpSpeed()))]]
        },
        {
            "from": 12,
            "to": 0,
            "speed": agent12.getUpSpeed() * 5,
            "label": String(Math.round(agent12.getUpSpeed())),
            "info": [["speed", String(Math.round(agent12.getUpSpeed()))]]
        },
        {
            "from": 13,
            "to": 0,
            "speed": agent13.getUpSpeed() * 5,
            "label": String(Math.round(agent13.getUpSpeed())),
            "info": [["speed", String(Math.round(agent13.getUpSpeed()))]]
        },
        {
            "from": 4,
            "to": 1,
            "speed": agent4.getUpSpeed() * 5,
            "label": String(Math.round(agent4.getUpSpeed())),
            "info": [["speed", String(Math.round(agent4.getUpSpeed()))]]
        },
        {
            "from": 5,
            "to": 1,
            "speed": agent5.getUpSpeed() * 5,
            "label": String(Math.round(agent5.getUpSpeed())),
            "info": [["speed", String(Math.round(agent5.getUpSpeed()))]]
        },
        {
            "from": 6,
            "to": 1,
            "speed": agent6.getUpSpeed() * 5,
            "label": String(Math.round(agent6.getUpSpeed())),
            "classes": ["error"],
            "info": [["speed", String(Math.round(agent6.getUpSpeed()))]]
        },
        {
            "from": 7,
            "to": 2,
            "speed": agent7.getUpSpeed() * 5,
            "label": String(Math.round(agent7.getUpSpeed())),
            "classes": ["inverted"],
            "info": [["speed", String(Math.round(agent7.getUpSpeed()))]]
        },
        {
            "from": 8,
            "to": 2,
            "speed": agent8.getUpSpeed() * 5,
            "label": String(Math.round(agent8.getUpSpeed())),
            "info": [["speed", String(Math.round(agent8.getUpSpeed()))]]
        },
        {
            "from": 9,
            "to": 2,
            "speed": agent9.getUpSpeed() * 5,
            "label": String(Math.round(agent9.getUpSpeed())),
            "info": [["speed", String(Math.round(agent9.getUpSpeed()))]]
        },
        {
            "from": 10,
            "to": 3,
            "speed": agent10.getUpSpeed() * 5,
            "label": String(Math.round(agent10.getUpSpeed())),
            "info": [["speed", String(Math.round(agent10.getUpSpeed()))]]
        },
        {
            "from": 11,
            "to": 3,
            "speed": agent11.getUpSpeed() * 5,
            "label": String(Math.round(agent11.getUpSpeed())),
            "info": [["speed", String(Math.round(agent11.getUpSpeed()))]]
        },
        {
            "from": 9,
            "to": 10,
            "speed": agent910.getUpSpeed() * 5,
            "label": String(Math.round(agent910.getUpSpeed())),
            "info": [["speed", String(Math.round(agent910.getUpSpeed()))]]
        }
    ];

    networkApp.ports.input.send(JSON.stringify({
        "name": "network",
        "size": [networkNode.offsetWidth, networkNode.offsetHeight],
        "position": [networkNode.offsetLeft, networkNode.offsetTop],
        "setNodes": nodes,
        "setEdges": edges
    }));
}

setInterval(run, 200);
