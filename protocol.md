---
layout: page
title: Protocol
permalink: /protocol/
feature-img: "assets/img/protocol.jpg"
---

The nodes and edges are based on delta messages. The `setNodes` and `setEdges`
commands are used when adding or updating elements. To remove a node or edge
the `removeNodes` and `removeEdges` commands are used. Node and edge objects
are the only ones based on updates. All properties for both the general model,
nodes and edges will be totally overwritten by new messages. All properties
will be cleared and only the new properties will be assigned to the object. If
you want a manual layout you will have to send this in every message. If you
want a node to have a specific class, the class has to be present in the nodes
class list every time an object with the node id is present in the `setNodes`
command. If the properties are not present anymore they will be reset to their
default values.

## Flags
All flag properties must be included when initializing the voog module.

`Elm.Main.embed(node, flags);`

## Flags Schema
```
{
    "$schema": "http://json-schema.org/draft-06/schema#",
    "title": "Input",
    "description": "Voog input object",
    "type": "object",
    "properties": {
        "webSocket": {
            "description": "Listen to input from a websocket, null if disabled",
            "type": "string"
        },
        "disableWindowResize": {
            "description": "If true, disable Voog's built-in resize detection",
            "type": "bool"
        }
    }
    "required": ["webSocket", "disableWindowResize"]
}
```

## Input Schema
```
{
    "$schema": "http://json-schema.org/draft-06/schema#",
    "title": "Input",
    "description": "Voog input object",
    "type": "object",
    "properties": {
        "name": {
            "description": "Name of graph",
            "type": "string"
        },
        "clear": {
            "description": "If true, remove all nodes and edges before applying remainder of current message",
            "type": "bool"
        },
        "size": {
            "description": "Width and height of graph",
            "type": "array",
            "items" {
                "type": "int"
            },
            "minItems": 2,
            "maxItems": 2
        },
        "position": {
            "description": "Position of graph",
            "type": "array",
            "items" {
                "type": "int"
            },
            "minItems": 2,
            "maxItems": 2
        },
        "layout": {
            "type": "string"
        },
        "nodeDistance": {
            "description": "Distance between nodes",
            "type": "float"
        },
        "attraction": {
            "description": "Edge spring coefficient in forced layout",
            "type": "float"
        },
        "repulsion": {
            "description": "Node repulsion coefficient in forced layout",
            "type": "float"
        },
        "center": {
            "description": "Center the graph on screen",
            "type": "bool"
        },
        "addMovement": {
            "description": "Objects moving along edges",
            "type": "array",
            "items": {
                "type": "object"
                "properties" : {
                    "from": {
                        "description": "Outgoing node id"
                        "type": "integer"
                    },
                    "to": {
                        "description": "Incoming node id"
                        "type": "integer"
                    },
                    "duration": {
                        "description": "Time in seconds from a to b"
                        "type": "float"
                    },
                    "icon": {
                        "description": "Icon of moving element"
                        "type": "string"
                    },
                    "classes": {
                        "description": "List of html classes",
                        "type": "array",
                        "items: {
                            "type": "string"
                        }
                    },
                },
                "required": ["from", "to", "duration", "icon"]
            }
        }
        "setNodes": {
            "description": "List of nodes",
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "id": {
                        "description": "The unique node identifier",
                        "type": "integer"
                    },
                    "info": {
                        "description": "List of key-value pairs",
                        "type": "array",
                        "items": {
                            "type": "array",
                            "items" {
                                "type": "string"
                            },
                            "minItems": 2,
                            "maxItems": 2
                        }
                    },
                    "classes": {
                        "description": "List of html classes",
                        "type": "array",
                        "items: {
                            "type": "string"
                        }
                    },
                    "name": {
                        "type": "string"
                    },
                    "shape": {
                        "description": "'rect' or 'circle', default: 'circle'",
                        "type": "string"
                    },
                    "image": {
                        "description": "Image url",
                        "type": "string"
                    }
                    "width": {
                        "description": "Radius for circle and width/2 for rect"
                        "type": "integer"
                    },
                    "height": {
                        "description": "Radius for circle and height/2 for rect"
                        "type": "integer"
                    },
                    "x": {
                        "description": "x position when using manual layout"
                        "type": "integer"
                    },
                    "y": {
                        "description": "y position when using manual layout"
                        "type": "integer"
                    },
                    "required": ["id"]
                }
            }
        },
        "setEdges": {
            "description": "List of edges",
            "type": "array",
            "items": {
                "type": "object",
                    "properties": {
                        "from": {
                            "description": "Outgoing node id"
                            "type": "integer"
                        },
                        "to": {
                            "description": "Incoming node id"
                            "type": "integer"
                        },
                        "info": {
                            "description": "List of key-value pairs"
                            "type": "array",
                            "items": {
                                "type": "array",
                                "items" {
                                    "type": "string"
                                },
                                "minItems": 2,
                                "maxItems": 2
                            }
                        },
                        "classes": {
                            "description": "List of html classes",
                            "type": "array",
                            "items: {
                                "type": "string"
                            }
                        },
                        "label": {
                            "type": "string"
                        },
                        "width": {
                            "type": "integer"
                        },
                        "speed": {
                            "description": "Speed of objects moving along edge",
                            "type": "integer"
                        },
                        "required": ["from", "to"]
                    }
            }
        },
        "removeNodes": {
            "description": "List of node ids",
            "type": "array",
            "items": {
                "type": "array",
                "items": {
                    "type": "int"
                }
            }
        },
        "removeEdges": {
            "description": "List of edge ids",
            "type": "array",
            "items": {
                "type": "array",
                "items": {
                    "items": {
                        "type": "array",
                        "items" {
                            "type": "int"
                        },
                        "minItems": 2,
                        "maxItems": 2
                    }
                }
            }
        }
    }
}
```
