# jea-code-gen generate the basic outline of common code structures

This is a tool to generate boiler plate code for various languages.

## interactive use

`jea-code-gen-prompt`

and enter: `class,dog,sleep,bark,dig,swim` and the class will be generayed in the current buffer.

enter `func,cat,meow,lick` and get a function named `cat` with the paramaters `meow` and `lick`.

## calling the functions directly (not normal use (use interactive version))

This is the python version, other languages will be supported.

`(jea-code-gen-class "dog" '("sleep", "bark", "dig", "swim"))`

the output in the current buffer will be:

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

`(jea-code-gen-func "cat" '("meaow lick))`

will insert into the current buffer:

```
    def cat(self, meow, lick):
        ""
        result = None
        return result
```

## verious languages

Call to turn on code gen for python.

`jea-code-gen-python`

more to follow
