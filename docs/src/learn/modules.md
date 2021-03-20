# Modules

Wipple has no concept of objects or records like other languages. Instead, you can use modules to group a set of variables together. Blocks that are evaluated on their own without being passed into a function also form a scope, but instead of returning the last statement, they return a module.

```wipple
person : {
    name : "Bob"
    favorite-color : "blue"
}

show person -- shows "<module>"
```

Modules are also functions, and accept a name as input, which will return the value of that name inside the module:

```wipple
show (person name) -- shows "Bob"
```

Blocks can be nested to form more complex modules:

```wipple
people : {
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

show (people alice id) -- shows "1"
show (people charlie name) -- shows "Charlie"
```

> **Note:** While modules are normally used just to declare variables, you can run any code inside a module! Only variables and other changes to the environment (explained in more detail later) are stored.
