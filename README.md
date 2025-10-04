<div align="center">
    <img src="logo.svg" alt="Quiver" width="300" />
    <p><em>A statically-typed functional programming language with structural typing, pattern matching, concurrent processes, and postfix-based syntax. Values flow left-to-right through transformation pipelines.</em></p>
    <br />
    <br />
    <br />
</div>


```quiver
// Define a recursive list type
type list = Nil | Cons[int, &];

// Import standard library
%"math" ~> math,

// Compute the sum of a list using tail recursion
#[list, int] {
  | ~> [Nil, acc] => acc
  | ~> [Cons[head, tail], acc] => {
    [head, acc]
    ~> math.add!
    ~> [tail, ~]
    ~> &
  }
} ~> sum',

#list { ~> [~, 0] ~> sum'! } ~> sum,

// Build and sum a list
Cons[1, Cons[2, Cons[3, Nil]]] ~> xs,
xs ~> sum!  // 6
```

> Run the example above in the REPL (`quiv repl`), or run the executable version in [examples/sum.qv](examples/sum.qv) with `quiv run examples/sum.qv`.

<br />

## Language features

- **Postfix syntax**: Data flows left-to-right through transformations
- **Structural typing**: Types are defined by their structure, not their names
- **Pattern matching**: Destructure and branch on values with expressive pattern syntax
- **Union types**: Model complex data with algebraic types
- **Tail recursion**: Efficient recursive algorithms via explicit tail-calls
- **Concurrent processes**: Erlang-inspired lightweight processes with typed message passing

See [docs/spec.md](docs/spec.md) for the complete language specification.

## Getting started

### Building from source

Quiver requires Rust 2024 edition or later. Clone the repository and build:

```bash
git clone https://github.com/joefreeman/quiver.git
cd quiver
cargo build --release
```

The compiled binary will be at `target/release/quiv`.

### Quick start

Run the REPL:

```bash
quiv repl
```

Execute a Quiver program:

```bash
quiv run program.qv
```

## CLI commands

- **`quiv repl`** - Start an interactive REPL session
- **`quiv run [FILE]`** - Run a Quiver program (`.qv` source or `.qx` bytecode)
  - `-e, --eval <CODE>` - Execute code directly from the command line
- **`quiv compile [FILE]`** - Compile source to bytecode
  - `-o, --output <FILE>` - Write output to file
  - `-d, --debug` - Include debug information in bytecode
  - `-e, --eval <CODE>` - Compile code directly from the command line
- **`quiv inspect <FILE>`** - Inspect compiled bytecode structure

All commands support reading from stdin when no file is specified.

## REPL commands

Within the REPL:

- `\?` - Show help message
- `\q` - Exit the REPL
- `\!` - Reset the environment
- `\v` - List all variables
- `\t` - List all type aliases

## License

MIT
