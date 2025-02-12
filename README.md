# zoar

[WIP] PL of reactive structs

[Zoar is currently under the design process and anything below are only visions for the language].

## Quick Overview

Zoar is a language aimed at being able to describe self-monitoring automata that themselves may contain automata.

## Struct, has a life of its own

Unlike other languages where structs are a static arrangement of data, Zoar structs can react when certain conditions are met and can modify itself. Because of this, defining functions are down via structs.

A struct is defined by one or several constructors together with rules how to modify the struct. A constructor is made of a on optional qualifier (UpperCamelCase) and a bare struct. A bare struct are fields inside curly braces `{field1, field2}`. Here are examples of several structs.

```haskell

struct cat = {name:string, age:int}

struct dog = GermanSheperd {name:string, age:int} | Poodle {name:string, age:int, happy:bool}

struct bird = {name:string, age:int}
    age >= 10   => Old bird
    age >= 0    => Young bird
    _           => Imaginary bird

-- Old bird means Old {name, age}
-- bird in `Old bird` is a shorthand for the entry constructor (more on this below)

struct factorial = {x:int}
    x < 0               => Error {}
    x == 0 || x == 1    => 1
    _                   => mul {x, factorial {sub {x, 1}}}

struct greet_dog = {d: dog}
    GermanSheperd {name, ...} = d   => IO.Print {concat {"Hello Big ", name}}
    Poodle {name, ...} = d          => IO.Print {concat {"Hello fancy ", name, " poodle"}}
```

Here, `{name:string, age:int}`, `{x:int}`, `{d:dog}`, `{"Hello Big", name}` are all bare structs. Notice that a field could be `name:string` which is a named field with a structdef speficied (string is a structdef or a definition of a struct), it could also be `"Hello Big"` which is a value, and additionally it could be named and has a value specified like `name="Daisy"`. All of these are valid fields.

`Poodle {name:string, age:int, happy:bool}` is an Example of a constructor and can be made by the following

```haskell

-- We can do this
struct a_poodle = dog.Poodle {"Daisy", 17}

-- But not this, because bird.Old is not an entry constructor
-- you cannot make bird.Old directly
struct an_old_bird = bird.Old {"Hai", 100} -- not allowed

-- we need to use one of the entry constructors
-- but in this case it only has the unqualified constructor
struct an_old_bird = bird {"Hai", 100}
```

A _qualifier_ in zoar is a way to describe a struct. You might have a `Big Red {5,4,6}` or `{Green "apple", Red "banana"}`. While you could potentially encode qualifiers inside the struct itself like having a field called `qualifier:string`, it offers a distinct struct in typechecking.

Qualifiers can be made on the fly within a struct definition and those qualifiers are scoped to that struct definition and so accessing `Poodle` of `dog` is done through `dog.Poodle`. So if `cat` also used `Poodle` inside its definition, `dog.Poodle` and `cat.Poodle` would then be treated different.

Further inspecting the snippet above we see `Poodle {name, ...} = d` which is called a _bind_ statement in zoar. A bind is an expression that can either succeed or fail in capturing the value of the right-hand-side. It is a destructuring which includes the qualifier in the destructuring.

Specifically the transform-rule syntax `<capture> => <struct>` is how to tell a struct how to transform. A `<capture>` is a bind, or conditional, or boolean, when those succeed or true, the struct transforms. Several transform rules can be specified.

Because a struct can transform, it is the way we can have functions (or methods) in zoar. Like the `factorial` struct above.

In zoar, think of a struct as something that has "a life of its own" that continues transforming until it can't no more.

## Intermediate Transformations and Intermediate Variables

Most of the time, we might need several function-local variables to compute something. Let us consider the `nCr` in math which has a numerator, and 2 factors in the denominator. If we need intermediate variables, in zoar, we can instead make intermediate transformations like below.

```haskell
struct nCr = {n:int, r:int}
    => {
        numerator=factorial {n},
        factor_1=factorial{r},
        factor_2=factorial{sub {n, r}},
        denominator=mul {factor_1, factor_2}
    }
        => div {numerator, denominator}
```

In zoar, fields can depend on previous fields in the same bare struct.

In the example above, we have defined `nCr` with a multi-stage transformation (of 1 branch).

### Type Signatures

In zoar, while the entry constructors do have their structdef (types) specified, the output is left to be infered and this is for a good reason. In zoar, it is possible to have several transformation rules and each different number of stages and it would be hard to keep track of this by hand, and then implement it by yourself. Here is an example.

```haskell

struct thing = {s:struct, size:Small {}| Big {}}
    Small {} = size     => Cute thing
    Big {} = size       => Normal thing

struct cat = {name:string, weight:int}
    name == "Leopard"   => Leopard cat
    weight >= 5         => thing {cat, thing.Big {}}

```

Above, `thing` would have the following Signature

```haskell
thing :: {s:struct, size: Small {} | Big {}}
    -> thing.Cute
    -> thing.Normal
```

As seen, both branches of the transformation are shown.

`cat` would have the following signature.

```haskell
cat :: {name:string, weight:int}
    -> Leopard cat
    -> thing
        -> thing.Cute
        -> thing.Normal
    -> {name:string, weight:int}
```

Here, the entire journey of the entry constructor is seen. There is bare struct at the end because the transform-rules are non-exhaustive and so the cat bare struct may be stuck as a bare struct.

Let's see another example:

```haskell
struct greet_person_then_give_computed_age = {name:string, age:int, prefix:Mr {} | Ms {} | Others {}}
    Mr {} = prefix => IO.Print {"Hello mr"} => mul {age, 2}
    Ms {} = prefix => IO.Print {"Hello ms"} => add {age, 2}
    Others {} = prefix => IO.Print {"Hello !"} => sub {age, 2}
```

This would have the following signature.

```haskell
greet_person_then_give_double_age :: {name:string, age:int, prefix:Mr {} | Ms {} Others {}}
    -> IO -> mul -> int
    -> IO -> add -> int
    -> IO -> sub -> int
```

As you can see, the `mul`, `add`, `sub` are themselves structs so it is reflected in the signature that the transformation passes through this.

When we have multiple constructors as such below,

```haskell

struct beverage = {name:string, has_ice:bool}

struct remove_ice = {{name, _}: beverage} => beverage {name, false}

struct cook =
    | WithHeat {s: beverage}
        s.has_ice => Warm {s}
        !s.has_ice => Evaporated s
    | WithCold {s: beverage}
        s.has_ice => no_ice = remove_ice {s} => WithCold {no_ice}
        !s.has_ice => Cold {s}
```

Here are the signatures of the above structs

```haskell

beverage :: {string, bool}

remove_ice :: {beverage} -> beverage

cook ::
    | WithHeat {beverage}
        -> Warm {beverage}
        -> Evaporated beverage
    | WithCold {beverage}
        -> remove_ice -> beverage -> WithCold {beverage}
        -> Cold {beverage}
```

Notice `Warm {s}` gives `Warm {beverage}` while `Evaporated s` gives `Evaporated beverage` in their signature. The signatures track the journey of the struct as it transform from one struct to another. Remember that "functions" are structs as well in zoar. And so, beverage above could become a `remove_ice` struct which then becomes a `beverage` struct which is then qualified with `WithCold`.

## Qualifiers have no order

Qualifiers can come in any order, and are "idempotent" so the same qualifier counts only as 1. So all of the following `thiing_i` are the same

```haskell

struct color = Red {s:struct} | Blue {s:struct} | Green {s:struct}
struct size = Small {s:struct} | Big {s:struct}

struct x = {1,2,3}
struct thing_1 = color.Red color.Blue size.Big {x}
struct thing_2 = color.Blue size.Big color.Red {x}
struct thing_3 = size.Big size.Big color.Blue color.Red {x}
-- the {} braces are implied, a b c d ... z is equivalent to a {b {c {... y {z}}}}
```

They all will become equivalent to something like `color.Red color.Blue size.Big {{1,2,3}}`.

This comes in handy and makes it possible to treat qualifiers as adjectives.

For example,

```haskell
struct cat = {name:string}
struct can_be_happy_cat = {c:cat, happy:bool}
    happy   => Happy c
    _       => Sad c
struct can_be_hungry_cat = {c:cat, hungry:bool}
    hungry  => Hungry c
    _       => Full c


-- name is not an optional parameter
-- name it has a fixed value
-- calling daisy_1 or daisy_2 will need this exact "daisy" as parameter
struct daisy_1 = {name:string="daisy"} => {c=cat {daisy_1}} => can_be_hungry_cat can_be_happy_cat {c}
struct daisy_2 = {name:string="daisy"} => {c=cat {daisy_2}} => can_be_happy_cat can_be_hungry_cat {c}


struct foo_1 = {d:struct=daisy_1 {"daisy"}}
    Hungry s = d => IO.Print {"Daisy1 is hungry"}
struct foo_2 = {d:struct=daisy_2 {"daisy"}}
    Hungry s = d => IO.Print {"Daisy2 is hungry"}
```

Here both `foo_1` and `foo_2` will print something even if `daisy_1` is `can_be_hungry_cat.Hungry can_be_happy.Happy` and `daisy_2` is `can_be_happy.Happy can_be_hungry.Hungry`.

### Constants

Above, we saw `daisy_1` would have needed a constant parameter `"daisy"`, but what if we wanted it to not have parameters. We could have done

```haskell
struct daisy_1 = {} => {name:string="daisy"} => {c=cat {daisy_1}} => can_be_hungry_cat can_be_happy_cat {c}
```

And so we can call `daisy_1 {}` and get back the same value as before, without having to do `daisy_1 {"daisy"}`.
