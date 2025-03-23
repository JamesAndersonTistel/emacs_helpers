# jea-code-gen for typescript

This is a tool to generate boiler plate code for javascript.

## interactive use
### one time
`jea-code-gen-use-javascript`

### use
`jea-code-gen-prompt`

## switches
enter `switch,x,1,2,3` will generate:

```
    switch(x) {
    case 1:
        break;
    case 2:
        break;
    case 3:
        break;
    default:
        break;
    }
```

enter `switch,pet,dog,cat,mouse` will generate:

```
    switch(pet) {
    case 'dog':
        break;
    case 'cat':
        break;
    case 'mouse':
        break;
    default:
        break;
    }
```

## dictionaries

enter: `dict,d1,a,1,bee,42.33,see,far`

and it will expand to:

```
let d1 = {
    'a': 1,
    'bee': 42.33,
    'see': 'far'
};
```
