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
- **`quiv run <FILE>`** - Run a Quiver program (`.qv` source or `.qx` bytecode)
  - `-e, --eval <CODE>` - Execute code directly from the command line
  - `-q, --quiet` - Print nothing but the program's own output
  - `--release` - Skip failure-provenance stamps (`run` compiles debug by default)
  - `-d, --detach` - Start the program on the server and return, printing its pid. It runs until it finishes or `quiv kill` stops it, and `quiv proc <PID>` shows its result
- **`quiv compile <FILE>`** - Compile source to bytecode, writing `foo.qv` to `foo.qx` beside it (`--eval` and stdin write to stdout)
  - `-o, --output <FILE>` - Write the bytecode here instead, or `-` for stdout
  - `--debug` - Include failure-provenance stamps in the bytecode
  - `--inline` - Inline imported modules into the entry unit, shaken to what it reaches
  - `-e, --eval <CODE>` - Compile code directly from the command line
- **`quiv inspect <FILE>`** - Print a program's bytecode, compiling it first if given source
  - `-e, --eval <CODE>` - Inspect code directly from the command line
  - `--debug` - Include failure-provenance stamps when compiling
- **`quiv format [PATH]...`** - Format files in place, walking any directory given (default: the current one)
  - `--check` - Write nothing; list what would change and exit non-zero
  - `-e, --eval <CODE>` - Format code directly from the command line
- **`quiv test <FILE.md>...`** - Run the Quiver code embedded in a Markdown document and check its `//=` assertions. Each `##` chapter is one accumulating session; exits non-zero if any check fails
- **`quiv server`** - Run the persistent server: one shared environment that client sessions connect to over a unix socket. `quiv run` and `quiv repl` spawn one automatically when none is listening, so this is only needed to run it in the foreground or to reach it from a browser
  - `--socket <PATH>` - Listen on this socket path instead of the per-user default
  - `--listen [<ADDRESS>]` - Additionally listen for browser clients on a loopback TCP address (default `127.0.0.1:2192`). Every request must carry the bearer token written beside the socket; non-loopback addresses are refused
  - `--allow-origin <ORIGIN>` - Allow this browser origin on the TCP listener (repeatable)
  - `quiv server status` / `quiv server stop` - Whether a server is listening, and stop it
- **`quiv proc [PID]`** - List the running server's processes, under a summary of their statuses and the workers: each one's status, owner, mailbox, owned resources, registry names and type; or, given a pid, show that process in detail
  - `--tree` - Show the ownership tree instead of a table
  - `-a, --all` - Include terminated processes not yet reclaimed
  - `--json` - Print JSON
  - `-w, --watch` - Keep the listing on screen, redrawn as it changes (Ctrl-C to quit)
- **`quiv kill <PID>...`** - Stop processes on the running server, with the subtrees they own

A `FILE` of `-` reads stdin instead: `run` and `inspect` accept source or bytecode there, telling them apart by content.

## REPL commands

Within the REPL:

- `\?` - Show help message
- `\q` - Exit the REPL
- `\!` - Reset the REPL
- `\r` - Reload project modules (keeps variables)
- `\v` - List all variables
- `\p` - List all processes, as an ownership tree
- `\p X` - Inspect process with ID `X`
- `\w` - List workers
- `\w X` - Inspect worker with ID `X`
- `\x` - Show the compile-time type of the last expression

## License

MIT
