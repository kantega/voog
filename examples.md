---
layout: page
title: Examples
permalink: /examples/
feature-img: "assets/img/sample_feature_img_3.png"
---

## Examples
### Basic Node
`addNodes` adds a list of node objects to the graph.
The minimum requirements for a node is an identifying integer: `{"id": 0}`, but here we also provide a name.

![Hello](/assets/img/hello.png)
```
{
    "addNodes": [
        {"id": 0, "name": "Hello"}
    ]
}
```
---
### Basic Edge
Nodes are connected with directed edges. The edge object only has two required fields, `from` and `to`.

![Hello World](/assets/img/hello_world.png)
```
{
    "addNodes": [
        {"id": 0, "name": "Hello"}, {"id": 1, "name": "World"}
    ],
    "addEdges": [
        {"from": 0, "to": 1}
    ]
}
```
---
### Remove
The remove object is quite simple, it only contains the ids of the objects to remove. For nodes that is a list fo integers and
for edges a list of integer pairs. When a node is removed all connected edges are also removed.

```
{
    "removeNodes": [
        1
    ],
    "removeEdges": [
        [0, 1]
    ]
}
```
