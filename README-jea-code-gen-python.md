# jea-code-gen for python

This is a tool to generate boiler plate code for python.

## interactive use
### one time
`jea-code-gen-use-python`

### use
`jea-code-gen-prompt`

## classes
enter: `class,dog,sleep,bark,dig,swim` will generate:

```
...snip license...

"""
"""
class Dog:
    ""

    def __init__(self):
        pass

    def sleep(self):
        ""
        result = None
        return result

    def bark(self):
        ""
        result = None
        return result

    def dig(self):
        ""
        result = None
        return result

    def swim(self):
        ""
        result = None
        return result
```

## functions
enter `func,cat,meow,sleep` will generate:

```
    def cat(self, meow, sleep):
        ""
        result = None
        return result
```

## switches
enter `switch,x,1,2,3` will generate:

```
    if x == 1:
        pass
    elif x == 2:
        pass
    elif x == 3:
        pass
    else:
        pass
```

enter `switch,pet,dog,cat,hamster` will generate:

```
    if pet == 'dog':
        pass
    elif pet == 'cat':
        pass
    elif pet == 'hamster':
        pass
    else:
        pass
```
