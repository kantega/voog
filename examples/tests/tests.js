class Test {
    constructor(node, app, name) {
        this.node = node;
        this.app = app;
        this.name = name;
        this.lastInput = {};
    }

    update() {
        this.app.ports.input.send(
            JSON.stringify(
                Object.assign({}, this.defaults(), this.lastInput, {'addMovement': []})
            )
        );
    }

    send(input) {
        this.lastInput = input;
        this.app.ports.input.send(JSON.stringify(Object.assign({}, this.defaults(), input)));
    }

    defaults() {
        return {
            "name": this.name,
            "size": [this.node.offsetWidth, this.node.offsetHeight],
            "position": [this.node.offsetLeft - (window.pageXOffset || document.documentElement.scrollLeft)
                , this.node.offsetTop - (window.pageYOffset || document.documentElement.scrollTop)]
        };
    }
}

function createTest(name) {
    var codename = name.replace(new RegExp(' ', 'g'), '');
    var node = document.createElement('div');
    node.id = codename;

    var app = Elm.Main.init({node: node, flags: {'webSocket': null, 'disableWindowResize': true}});
    node.classList.add('test');
    var test = new Test(node, app, codename);

    var title = document.createElement('p');
    title.innerHTML = name;

    node.appendChild(title);
    document.getElementById('tests').appendChild(node);

    node.addEventListener('mousewheel', function (e) {
        e.preventDefault();
    });

    window.addEventListener('resize', function (event) {
        test.update();
    });
    window.addEventListener('scroll', function (event) {
        test.update();
    });

    return new Promise(function (callback) {
        setTimeout(function () {
            callback(function (input) {
                test.send(input)
            });
        }, 100);
    });
}

function edges(edges) {
    var voogEdges = [];
    for (var i = 0; i < edges.length; i++) {
        voogEdges.push({'from': edges[i][0], 'to': edges[i][1]});
    }
    return voogEdges;
}
