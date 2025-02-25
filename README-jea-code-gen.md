# jea-code-gen

This is a tool to generate boiler plate code for various languages.

## generate the basic outline of a class

see below for normal interactive use.

`jea-code-gen-class`

If the arguments are: 

`(jea-code-gen-class "dog" '("sleep", "bark", "dig", "swim"))`

the output will be:

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

## interactive use

`jea-code-gen-prompt`

and enter: `class,dog,sleep,bark,dig,swim` and the class will be generayed in the current buffer.

## verious languages

`jea-code-gen-python`

Call to turn on code gen for python.
