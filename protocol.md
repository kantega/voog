---
layout: page
title: Protocol
permalink: /protocol/
feature-img: "assets/img/protocol.jpg"
---

The protocol is based on delta messages. The `setNodes` and `setEdges` commands are used when adding or updating elements.
To remove a node or edge the `removeNodes` and `removeEdges` commands are used.
Although the node and edge objects are based on deltas, their attributes are not. When updating an existing object
with the add command the object will be totally overwritten. All attributes will be cleared and only the new attributes
will be assigned to the object.

## Schema
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
                    "size": {
                        "description": "Radius for circle and width/2 for rect"
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
