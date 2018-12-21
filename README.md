![voog](https://cdn.rawgit.com/kantega/voog/gh-pages/assets/img/logo_header.svg)

Voog is a stream network visualiser written in elm.  
Documentation and examples are available on our [GitHub page](https://kantega.github.io/voog).

## Getting started
Create a div element and bind Voog to the element by its id.

```html
<html>
  <body>
      <div id="main"></div>
  </body>
</html>
```

### With npm
`npm install --save @kantega/voog`
```javascript
import {Voog} from "@kantega/voog";
const voog = Voog.init('main');
voog.send({ .. });
```

### Pure browser
```html
<script src="voog.js"></script>
<script>
  const voog = Voog.init('main');
  voog.send({ .. });
</script>
```
