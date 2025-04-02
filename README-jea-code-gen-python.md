# jea-code-gen for python

This is a tool to generate boiler plate code for python.

## interactive use
### one time
`jea-code-gen-use-python`

### use
`jea-code-gen-prompt`

## classes

(note the prefix arg)

enter: `class,dog,ssleep,ibark,bdig,sswim` will generate:

```
...snip license...

"""
"""
class Dog:
    ""

    def __init__(self, sleep: str, bark: int, dig: bool, swim: str):
        self._sleep = sleep
        self._bark = bark
        self._dig = dig
        self._swim = swim


if __name__ == '__main__':
    import doctest
    doctest.testmod()
```

## functions
enter `func,get_car,smake,smodel` will generate:

```
def get_car(self, make: str, model: str):
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

## dictionaries
enter `dict,d1,a,1,bee,42.33,see,far` will generate:

```
d1 = {
    'a': 1,
    'bee': 42.33,
    'see': 'far'
}
```

## unit tests
evaluate function `(jea-code-gen-test-python)` 

or evaluate file: [code](./jea-code-gen-python-test.el)

