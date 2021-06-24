import Runtime

// FIXME(HACK): Use implementation in Runtime once finished (https://github.com/wickwirew/Runtime/pull/91)

private struct ProtocolTypeContainer {
    let type: Any.Type
    let witnessTable: Int
}

private protocol Getters {}
private extension Getters {
    static func get(from pointer: UnsafeRawPointer) -> Any {
        return pointer.assumingMemoryBound(to: Self.self).pointee
    }
}

private func getters(type: Any.Type) -> Getters.Type {
    let container = ProtocolTypeContainer(type: type, witnessTable: 0)
    return unsafeBitCast(container, to: Getters.Type.self)
}

internal extension TypeInfo {
    func ensureIsCLikeEnum() {
        precondition(self.kind == .enum)
        precondition(!self.cases.isEmpty, "Enums with no cases aren't supported")
        precondition(self.numberOfPayloadEnumCases == 0, "Enums with associated values aren't supported")
    }
}

internal func createCLikeEnum<T>(case caseName: String) -> T {
    createCLikeEnum(of: T.self, case: caseName) as! T
}

internal func createCLikeEnum(of type: Any.Type, case caseName: String) -> Any {
    let info = try! typeInfo(of: type)
    info.ensureIsCLikeEnum()

    let pointer = UnsafeMutableRawPointer.allocate(byteCount: info.size, alignment: info.alignment)
    defer { pointer.deallocate() }

    func getEnum() -> Any {
        getters(type: type).get(from: pointer)
    }

    if info.cases.count == 1 {
        precondition(caseName == info.cases[0].name)

        return getEnum()
    }

    let index = info.cases.firstIndex(where: { $0.name == caseName })!

    switch info.size {
    case MemoryLayout<UInt8>.size:
        pointer.storeBytes(of: UInt8(exactly: index)!, as: UInt8.self)
    case MemoryLayout<UInt16>.size:
        pointer.storeBytes(of: UInt16(exactly: index)!, as: UInt16.self)
    default:
        // Enum has more cases than can fit into a UInt16
        fatalError("Enum has too many cases")
    }

    return getEnum()
}

func caseName<T>(ofCLikeEnum value: T) -> String {
    let info = try! typeInfo(of: T.self)
    info.ensureIsCLikeEnum()

    if info.numberOfEnumCases == 1 {
        return info.cases[0].name
    } else {
        let index: Int

        switch info.size {
        case MemoryLayout<UInt8>.size:
            index = Int(unsafeBitCast(value, to: UInt8.self))
        case MemoryLayout<UInt16>.size:
            index = Int(unsafeBitCast(value, to: UInt16.self))
        default:
            // Enum has more cases than can fit into a UInt16
            fatalError("Enum has too many cases")
        }

        return info.cases[index].name
    }
}
