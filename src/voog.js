'use strict';

import "./system_style.css";
import "./default_style.css";

import {Elm as ElmVoog} from './Main.elm';

export class Voog {
    constructor(app, node) {
        this.app = app;
        this.node = node;

        node.addEventListener('mousewheel', (e) => e.preventDefault());
        window.addEventListener('resize',  () => this.sendSizeAndPosition());
        window.addEventListener('scroll', () => this.sendSizeAndPosition());
    }

    static init(elementId) {
        const node = document.getElementById(elementId);
        const app = ElmVoog.Main.init(
            {
                node: node,
                flags: {},
            }
        );

        return new Voog(app, node);
    }

    send(message) {
        this.app.ports.input.send(
            JSON.stringify(message)
        );
    }

    sendSizeAndPosition() {
        this.send({
            "size": [this.node.offsetWidth, this.node.offsetHeight],
            "position": [this.node.offsetLeft - (window.pageXOffset || document.documentElement.scrollLeft)
                , this.node.offsetTop - (window.pageYOffset || document.documentElement.scrollTop)]
        });
    }
}
