import Wipple

public func bind(_ function: @escaping () -> Void) -> Variable.Compute {
    { env, stack in
        function()
        return .empty
    }
}

public func bind<T: ValueConvertible>(_ function: @escaping () -> T) -> Variable.Compute {
    { env, stack in
        let result = function()
        return try result.toValue(env, stack)
    }
}

public func bind<A: ValueConvertible>(_ function: @escaping (A) -> Void) -> Value {
    Value(Function { value, env, stack in
        let a = try A.fromValue(value.evaluate(env, stack), env, stack)

        function(a)
        return .empty
    })
}

public func bind<A: ValueConvertible, T: ValueConvertible>(_ function: @escaping (A) -> T) -> Value {
    Value(Function { value, env, stack in
        let a = try A.fromValue(value.evaluate(env, stack), env, stack)

        let result = function(a)
        return try result.toValue(env, stack)
    })
}

public func bind<A: ValueConvertible, B: ValueConvertible>(_ function: @escaping (A, B) -> Void) -> Value {
    Value(Function { value, env, stack in
        let a = try A.fromValue(value.evaluate(env, stack), env, stack)

        return Value(Function { value, env, stack in
            let b = try B.fromValue(value.evaluate(env, stack), env, stack)

            function(a, b)
            return .empty
        })
    })
}

public func bind<A: ValueConvertible, B: ValueConvertible, T: ValueConvertible>(_ function: @escaping (A, B) -> T) -> Value {
    Value(Function { value, env, stack in
        let a = try A.fromValue(value.evaluate(env, stack), env, stack)

        return Value(Function { value, env, stack in
            let b = try B.fromValue(value.evaluate(env, stack), env, stack)

            let result = function(a, b)
            return try result.toValue(env, stack)
        })
    })
}

public func bind<A: ValueConvertible, B: ValueConvertible, C: ValueConvertible>(_ function: @escaping (A, B, C) -> Void) -> Value {
    Value(Function { value, env, stack in
        let a = try A.fromValue(value.evaluate(env, stack), env, stack)

        return Value(Function { value, env, stack in
            let b = try B.fromValue(value.evaluate(env, stack), env, stack)

            return Value(Function { value, env, stack in
                let c = try C.fromValue(value.evaluate(env, stack), env, stack)

                function(a, b, c)
                return .empty
            })
        })
    })
}

public func bind<A: ValueConvertible, B: ValueConvertible, C: ValueConvertible, T: ValueConvertible>(_ function: @escaping (A, B, C) -> T) -> Value {
    Value(Function { value, env, stack in
        let a = try A.fromValue(value.evaluate(env, stack), env, stack)

        return Value(Function { value, env, stack in
            let b = try B.fromValue(value.evaluate(env, stack), env, stack)

            return Value(Function { value, env, stack in
                let c = try C.fromValue(value.evaluate(env, stack), env, stack)

                let result = function(a, b, c)
                return try result.toValue(env, stack)
            })
        })
    })
}
