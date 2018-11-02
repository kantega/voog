---
layout: page
title: Getting Started
permalink: /getting_started/
feature-img: "assets/img/circles.jpg"
---

## Getting Started

### Basic Node
`setNodes` adds a list of node objects to the graph.
The minimum requirements for a node is an identifying integer: `{"id": 0}`, but
here we also provide a name.

![Hello](/voog/assets/img/hello.png)
```
{
    "setNodes": [
        {"id": 0, "name": "Hello"}
    ]
}
```
---

### Basic Edge
Nodes are connected with directed edges. An edge object has only two required
fields, `from` and `to`. If the referenced nodes doesn't exist, they are
created automatically.

![Hello World](/voog/assets/img/hello_world.png)
```
{
    "setNodes": [
        {"id": 0, "name": "Hello"}, {"id": 1, "name": "World"}
    ],
    "setEdges": [
        {"from": 0, "to": 1}
    ]
}
```
---

### Remove
The remove object is quite simple, it only contains the identifiers of the
objects to remove. For nodes that is a list of integers and for edges a list of
pairs of integer. When a node is removed all connected edges are also removed.

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
---

### Edge flow
Voog supports two types of information flow along edges: a constant flow of
dashes or a single object. To initate a constant flow, set the `speed`
property.  To add a moving object, use the `addMovement` command. Voog supports
the following icons by default: 'dot', 'message' and 'plane'. The duration, in
seconds, is the time spent moving from node A to B.
```
{
    "setEdges": [
        {"from": 0, "to": 1, "speed": 40}
    ]
}
```
```
{
    "addMovement": [
        {"from": 0, "to": 1, "icon": "message", "duration": 3}
    ]
}
```
![Movement](/voog/assets/img/message.png)

### Layout
Voog has three primary layout algorithms: `layered`, `manual`, and `zero`.
Additional options can be specified by a dot.  With the `forced` specification
the graph will initially be laid out by the main layout. Then forces will move
nodes away from other nodes, and edges pull the connected nodes together. The
`forceDampFactor` adjusts how fast the motion converges to a fixed state.

```
{
    "setEdges": [
        {"from": 0, "to": 1},
        {"from": 0, "to": 2},
        {"from": 0, "to": 3},
    ],
    "layout": "layered.forced",
    "forceDampFactor": 0.95,
}
```
