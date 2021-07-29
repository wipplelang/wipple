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
            <td>
                <pre><code>(version "1.2.3")</code></pre>
            </td>
            <td>
                The version of the interpreter this binary is compiled for; must be the first
                command in the file
            </td>
        </tr>
        <tr>
            <td>
                <pre><code>(block ...)</code></pre>
            </td>
            <td>
                Define a block with a sequence of instructions; the last block is the program
                entrypoint
            </td>
        </tr>
        <tr>
            <td>
                <pre><code>(data num 3.14)</code></pre>
                <br />
                <pre><code>(data int 42)</code></pre>
                <br />
                <pre><code>(data bin 101010)</code></pre>
                <br />
                <pre><code>(data str "Hello world")</code></pre>
                <br />
                <pre><code>(data true)</code></pre>
                <br />
                <pre><code>(data false)</code></pre>
                <br />
                <pre><code>(data raw "deadbeef")</code></pre>
            </td>
            <td>Define data of various types</td>
        </tr>
        <tr>
            <td>
                <pre><code>(extern "object" "symbol")</code></pre>
            </td>
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
            <td>
                <pre><code>(def foo $1)</code></pre>
            </td>
            <td>Create an alias to a variable index</td>
        </tr>
        <tr>
            <td>
                <pre><code>(def mydata data ...)</code></pre>
            </td>
            <td>Define data with an alias to its index</td>
        </tr>
        <tr>
            <td>
                <pre><code>(def myblock block ...)</code></pre>
            </td>
            <td>Define a block with alias to its index</td>
        </tr>
        <tr>
            <td>
                <pre><code>(def myfunc extern ...)</code></pre>
            </td>
            <td>Define an external refrence with an alias to its index</td>
        </tr>
        <tr>
            <td>
                <pre><code>(def foo)</code></pre>
            </td>
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
            <td>
                <pre><code>(const ...)</code></pre>
            </td>
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
            <td>
                <pre><code>(enter block inputs...)</code></pre>
            </td>
            <td>Enter a block, coping the provided inputs into the block's stack frame</td>
        </tr>
        <tr>
            <td>
                <pre><code>(exit outputs...)</code></pre>
            </td>
            <td>
                Exit the current block, copying the provided variables out of the block's stack
                frame
            </td>
        </tr>
        <tr>
            <td>
                <pre><code>(call func inputs...)</code></pre>
            </td>
            <td>Call an external function with the provided variables</td>
        </tr>
        <tr>
            <td>
                <pre><code>(use global)</code></pre>
            </td>
            <td>
                Copy the data at the provided global variable (<code>data</code> item) into the next
                variable on the stack frame
            </td>
        </tr>
        <tr>
            <td>
                <pre><code>(if condition (then...) (else...))</code></pre>
            </td>
            <td>
                Read the boolean stored at `condition`, executing the first list of instructions if
                nonzero and the second list of instructions if zero
            </td>
        </tr>
    </tbody>
</table>
