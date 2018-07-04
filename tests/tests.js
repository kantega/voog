class Test {
    constructor(node, app, name) {
        this.node = node;
        this.app = app;
        this.name = name;
        this.lastInput = {};
    }

    resize() {
        this.app.ports.input.send(JSON.stringify(Object.assign({}, this.defaults(), this.lastInput)));
    }

    send(input) {
        this.lastInput = input;
        this.app.ports.input.send(JSON.stringify(Object.assign({}, this.defaults(), input)));
    }

    defaults() {
        return {
            "name": this.name,
            "size": [this.node.offsetWidth, this.node.offsetHeight],
            "position": [this.node.offsetLeft, this.node.offsetTop]
        };
    }
}

function createTest(name) {
    var codename = name.replace(new RegExp(' ', 'g'), '');
    var node = document.createElement('div');
    node.classList.add('test');
    node.id = codename;

    var app = Elm.Main.embed(node, {'webSocket': null, 'disableWindowResize': true});
    var test = new Test(node, app, codename);

    var title = document.createElement('p');
    title.innerHTML = name;

    node.appendChild(title);
    document.getElementById('tests').appendChild(node);

    node.addEventListener('mousewheel', function (e) {
        e.preventDefault();
    });

    window.addEventListener('resize', function(event){
        test.resize();
    });

    return new Promise(function (callback) {
        setTimeout(function() {
            callback(function(input) {test.send(input)});
        }, 100);
    });
}
