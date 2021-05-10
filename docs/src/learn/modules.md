# Modules

Wipple has no concept of objects or records like other languages. Instead, you can use modules to "capture" a scope, including any variables defined within. You can create a module by using the `new` function. `new` works just like `do`, except it returns its scope instead of the evaluated value.

```wipple
person : new {
    name : "Bob"
    favorite-color : "blue"
}

show person -- <module>
```

Modules are also functions, and accept a name as input, which will return the value of the variable with that name inside the module:

```wipple
show (person name) -- Bob
```

Blocks can be nested to form more complex modules. Any blocks declared inside a block passed to `new` are implicitly evaluated as modules, so you only need `new` on the outermost block:

```wipple
people : new {
    alice : {
        id : 1
        name : "Alice"
    }

    bob : {
        id : 2
        name : "Bob"
    }

    charlie : {
        id : 3
        name : "Charlie"
    }
}

show (people alice id) -- 1
show (people charlie name) -- Charlie
```

> **Note:** While modules are normally used just to declare variables, you can run any code inside a module! The module only stores any declared variables and other changes to the environment (explained in more detail later).
