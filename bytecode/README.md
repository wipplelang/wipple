# Wipple Bytecode

Use this crate to manage Wipple bytecode files, in both textual (`.wplb`) and binary (`.wplc`) form. There is also a command line interface accessible via `cargo run`. For examples, see the `examples` folder.

## Specification

Wipple bytecode files are Wipple programs that have been compiled ot a format executable by the low-level interpreter (or eventually compiled further into eg. WebAssembly).

Bytecode files come in two formats, `.wplb` (textual) and `.wplc` (binary), and are comprised of the following commands:

<table>
    <thead>
        <tr>
            <th>Command</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td><code>(version "1.2.3")</code></td>
            <td>
                The version of the interpreter this binary is compiled for; must be the first
                command in the file
            </td>
        </tr>
        <tr>
            <td><code>(block ...)</code></td>
            <td>
                Define a block with a sequence of instructions; the last block is the program
                entrypoint
            </td>
        </tr>
        <tr>
            <td>
                <code>(data num 3.14)</code><br />
                <code>(data int 42)</code><br />
                <code>(data bin 101010)</code><br />
                <code>(data str "Hello world")</code><br />
                <code>(data true)</code><br />
                <code>(data false)</code><br />
                <code>(data raw "deadbeef")</code><br />
            </td>
            <td>Define data of various types</td>
        </tr>
        <tr>
            <td><code>(extern "object" "symbol")</code></td>
            <td>Define a reference to an external function</td>
        </tr>
    </tbody>
</table>

Textual bytecode files can also define aliases for convenience (binary files only use numeric aliases):

<table>
    <thead>
        <tr>
            <th>Command</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td><code>(def foo $1)</code></td>
            <td>Create an alias to a variable index</td>
        </tr>
        <tr>
            <td><code>(def mydata data ...)</code></td>
            <td>Define data with an alias to its index</td>
        </tr>
        <tr>
            <td><code>(def myblock block ...)</code></td>
            <td>Define a block with alias to its index</td>
        </tr>
        <tr>
            <td><code>(def myfunc extern ...)</code></td>
            <td>Define an external refrence with an alias to its index</td>
        </tr>
        <tr>
            <td><code>(def foo)</code></td>
            <td>Define an alias to the next variable index in a block</td>
        </tr>
    </tbody>
</table>

In addition, textual bytecode files can also use `const` to declare an inline reference to data:

<table>
    <thead>
        <tr>
            <th>Command</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td><code>(const ...)</code></td>
            <td>Define inline data which can be used in place of a variable or index</td>
        </tr>
    </tbody>
</table>

The available instructions for use in a block are as follows:

<table>
    <thead>
        <tr>
            <th>Command</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td><code>(enter block inputs...)</code></td>
            <td>Enter a thunk, copying the provided inputs into the resolved block's stack frame</td>
        </tr>
        <tr>
            <td><code>(exit outputs...)</code></td>
            <td>
                Exit the current block, copying the provided variables out of the block's stack
                frame
            </td>
        </tr>
        <tr>
            <td><code>(call func inputs...)</code></td>
            <td>Call an external function with the provided variables</td>
        </tr>
        <tr>
            <td><code>(use global)</code></td>
            <td>
                Copy the data at the provided global variable (<code>data</code> item) into the next
                variable on the stack frame
            </td>
        </tr>
        <tr>
            <td><code>(thunk block inputs...)</code></td>
            <td>
                Wrap a block index and copies of the provided inputs into a thunk value that can be
                resolved dynamically, and store the value into the next variable on the stack
            </td>
        </tr>
    </tbody>
</table>
