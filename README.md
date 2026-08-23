<div align="center">
    <img src="logo.svg" alt="Quiver" width="300" />
    <p><em>A statically-typed functional programming language with structural typing, pattern matching, lightweight processes, typed message passing, and a pipeline syntax.</em></p>
    <a href="https://quiver.run">Try Quiver in the online REPL</a>
    <br />
    <br />
    <br />
</div>


```quiver program
// Define a recursive list type (matching the list module's)
'list<'t> = Nil | Cons['t, ^]

// Define a function to compute the sum of a list using tail recursion
sum = #['list<'int>, (acc): 'int = 0] {
  | =[Nil, acc: acc] => acc
  | =[Cons[head, tail], acc: acc] => {
    %num.add [head, acc] ~> ^ [tail, ~]
  }
}

// Entrypoint of the program
#[] {
  // Build a list (using the list module's dialect), and sum it
  %list{ 1, 2, 3 } ~> sum [~]  //= 6
}
```

> Run the example above in the REPL (`quiv repl`, or at [quiver.run](https://quiver.run)), or run the executable version in [examples/sum.qv](examples/sum.qv) with `quiv run examples/sum.qv`.

<br />

## Language features

- **Pipeline syntax**: Data flows left-to-right through `~>`-separated transformations
- **Structural typing**: Types are defined by their structure, not their names
- **Pattern matching**: Destructure and branch on values with expressive pattern syntax
- **Union types**: Model complex data with algebraic types
- **Tail recursion**: Efficient recursive algorithms via explicit tail-calls
- **Concurrent processes**: Erlang-inspired lightweight processes with typed message passing

See [docs/guide.md](docs/guide.md) for the language guide.

## Getting started

### Building from source

Clone the repository and build:

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
- **`quiv format [PATH]...`** - Format files in place, walking any directory given (default: the current one)
  - `--check` - Write nothing; list what would change and exit non-zero
  - `-e, --eval <CODE>` - Format code directly from the command line

Commands read from stdin when no file is specified (`quiv format` spells that `-`).

## REPL commands

Within the REPL:

- `\?` - Show help message
- `\q` - Exit the REPL
- `\!` - Reset the environment
- `\v` - List all variables
- `\p` - List all processes
- `\p X` - Inspect process with ID `X`

## License

MIT
