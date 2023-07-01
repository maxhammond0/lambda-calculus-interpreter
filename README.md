## An interpreter for pure lambda calculus defined by my own syntax
The purpose of this is to learn both lambda calculus and get more familiar
with haskell

## Syntax is as follows:
```
<expression> := <name> | <function> | <application
<function>   := (>=) <name> . <expression>
<application := <expression> <expression>
```
You can also perform substituion by defining them with the = operator:
```
I = ((>=)x . x)
```

# An example of the y combinator:
```
Y = (>=)f.((>=)x.f(x x))((>=)x.f(x x))
```
