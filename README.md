![voog](https://cdn.rawgit.com/kantega/voog/gh-pages/assets/img/logo.svg)

## Getting started
### With npm
`npm install --save @kantega/voog`
```javascript
import {Voog} from "@kantega/voog";
const voog = Voog.init('main');
voog.send({ .. });
```

### Pure browser
```html
<html>
  <body>
      <div id="main"></div>
  </body>

  <script src="voog.js"></script>
  <script>
    const voog = Voog.init('main');
    voog.send({ .. });
  </script>
</html>
```

Read more on our [GitHub page](https://kantega.github.io/voog)
