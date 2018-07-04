var nodes = [];
var apps = [];

function createTest(name) {
    var node = document.createElement('div');
    node.classList.add('test');
    node.id = name;
    nodes.push(node);

    var app = Elm.Main.embed(node, {'webSocket': null});
    apps.push(app);

    var title = document.createElement('p');
    title.innerHTML = name;

    node.appendChild(title);
    document.body.appendChild(node);

    var codename = name.replace(new RegExp(' ', 'g'), '');

    return new Promise(function (callback) {
        setTimeout(function() {
            callback(function(input) {
                defaults = {
                    "name": codename,
                    "size": [node.offsetWidth, node.offsetHeight],
                    "position": [node.offsetLeft, node.offsetTop]
                };
                app.ports.input.send(JSON.stringify(Object.assign({}, defaults, input)));
            });
        }, 100);
    });
}
