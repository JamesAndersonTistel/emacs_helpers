# jea-code-gen for typescript

This is a tool to generate boiler plate code for various languages. examples here are python but will be doing other languages later.

## interactive use
### one time
`jea-code-gen-use-python`

### use
`jea-code-gen-prompt`

## classes

(note the prefix on the variables)

enter: `class,dog,ssleep,nbark,ndig,sswim` will generate:

```
/*
... snip license ...
*/


/**
 *
 */
class Dog {
  /**
   *
   */
  sleep: string;

  /**
   *
   */
  bark: number;

  /**
   *
   */
  dig: boolean;

  /**
   *
   */
  swim: string;


  constructor(sleep: string, bark: number, dig: boolean, swim: string) {
    this.sleep = sleep;
    this.bark = bark;
    this.dig = dig;
    this.swim = swim;
    
  }
}
```
