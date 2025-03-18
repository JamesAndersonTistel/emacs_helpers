# jea-code-gen for react

This is a tool to generate boiler plate code for react.

## interactive use
### one time
`jea-code-gen-use-react`

### use
`jea-code-gen-prompt`

## classes

(note the prefix on the variables)

enter: `class,dog,sbark,idigDown,fswim,bhappy` will generate:

```
... snip license ...
import { useState } from "react";

export default function Dog() {
    const [bark, setBark] = useState("");
    const [digDown, setDigDown] = useState(0);
    const [swim, setSwim] = useState(0.0);
    const [happy, setHappy] = useState(false);

    return (<></>);
}
```
