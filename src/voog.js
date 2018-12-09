'use strict';

require('./index.html');
import "./system_style.css";
import "./default_style.css";

import {Elm as ElmVoog} from './Main.elm';

export class Voog {
    constructor(app, node) {
        this.app = app;
        this.node = node;
    }

    static init(elementId) {
        const node = document.getElementById(elementId);
        const app = ElmVoog.Main.init(
            {
                node: node,
                flags: {
                    'webSocket': null,
                    'disableWindowResize': true,
                }
            }
        );

        return new Voog(app, node);
    }

    send(message) {
        this.app.ports.input.send(
            JSON.stringify(message)
        );
    }
}
