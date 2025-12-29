# lambda-stuff

this is a very simple languge with eager evaluation.
there isnt that much to things we dont have boolean types at all just 0 and 1 with * and +

# basic syntax
vars
```
varname = 2
```

basic arithmetic +-\*\/
```
2*3
```

functions and closures
```
f = /a /b a+b
(f 1 1)
```

# recursion
```
Z = /f ((/x (f (/v ((x x) v)))) (/x (f (/v ((x x) v))))))
```
this one liner gives you a z combinator that can be used this way

```
fact = (Z (/self (/n if n then n * (self (n - 1)) else 1)))
```

for getting recursion to work.