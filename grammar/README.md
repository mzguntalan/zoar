# Overview

This is a sample of the new syntax

```haskell

struct cat = {name:string, age:int};

struct dog = GermanSheperd {name:string, age:int} | Poodle {name:string, age:int};

struct bird = {name:string, age:int} [
    gt {age, 10}    => Old bird,
    gt {age, 0}     => Young bird,
    bool.True       => Imaginary bird,
];

struct factorial = {x:int} [
    lt {x, 0}   => Error {},
    eq {x, 0}   => eq {x, 1}    => 1,
    bool.True   => mul {x, factorial {sub {x, 1}}},
];
```

```ebnf
letter = ...
upper_case = ...
lower_case = ...
upper_camel_case = ...
number = ...
symbol = "{" | "}" | "=>" | "|" | "=" | "," | "." | "\"" | ";"

keyword = "struct"
S = {" " | "\n" | "\t"}
struct_definition = "struct", S, varname, S, "=", S, constructor {, S, "|", constructor}, ";"

varname = letter, {letter | number | "_"}
constructor = (qualifier, S), bare_struct (, S, transform_rules | direct_transform)

qualifier = upper_camel_case

direct_transform = "=>", S, constructor
transform_rules = "[", {S, struct_call | bind, S, "=>", S, constructor, ","} "]"

bare_struct = "{", S, struct_field, S, {",", S, struct_field, S, (",")} ,"}"
struct_field = without_set | with_set
without_set = varname, S, ":", S, varname
with_set = without_set, S, "=", S, value

struct_construction = varname,(".", qualifier), S, bare_struct_construction
bare_struct_construction = "{", S, value, S (",", value, S), "}"
value = number | string | struct_construction




```
