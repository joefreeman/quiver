# %exec

Operating-system programs, run as children of the host. `run` runs one to completion and
answers its output; `spawn` starts one and answers the child and its three standard streams,
for a program that is talked to while it runs. Programs are started directly, never through a
shell — a caller who wants one runs `sh -c`.

```quiver ignore
'exit    = Exited[source: +Child, code: 'int] | Signalled[source: +Child, signal: 'int]
'signal  = Hup | Int | Quit | Kill | Usr1 | Usr2 | Term
'change  = Set['%str, '%str] | Unset['%str]
'output  = [stdout: 'bin, stderr: 'bin]
'failure = Failed[code: 'int, stdout: 'bin, stderr: 'bin] | Killed[signal: 'int, stdout: 'bin, stderr: 'bin]
```

## Running a program

`run` takes the program and its arguments, feeds it `stdin:` (nothing, by default), and
answers what it wrote to stdout and stderr — when it exits with status 0.

```quiver
%exec.run ["echo", %list{ "hello", "world" }] ~> =(stdout: ~) ~> Str[~]   //= "hello world\n"
%exec.run ["tr", %list{ "a-z", "A-Z" }, stdin: "shout" ~> .0] ~> =(stdout: ~) ~> Str[~]   //= "SHOUT"
```

The input is written while the output is read, so neither waits on the other however much
there is of either:

```quiver
%exec.run ["cat", stdin: %bin.new 300000] ~> =(stdout: ~) ~> %bin.length   //= 300000
```

A program that ran but did not succeed fails as a nil carrying `:error`: `Failed` with its
exit status, or `Killed` with the signal that ended it — the output it wrote alongside, as
that is usually where it says why.

```quiver
r = %exec.run ["sh", %list{ "-c", "echo nope >&2; exit 4" }]
r                                                   //= []
r ~> :error<'%exec.failure> ~> =Failed(code: ~)     //= 4
r ~> :error<'%exec.failure> ~> =Failed(stderr: ~) ~> Str[~]   //= "nope\n"
%exec.run ["sh", %list{ "-c", "kill -9 $$" }] ~> :error<'%exec.failure> ~> =Killed(signal: ~)   //= 9
```

A program that cannot be started at all answers the `'%io.error` failure, `NotFound` for one that
is not there:

```quiver
%exec.run ["quiver-no-such-program"] ~> :error<'%io.error> ~> =IoError(kind: ~)   //= NotFound
```

## Finding programs

A name is looked up on `PATH` — the one the child will see — through absolute entries only,
so an empty or relative entry never runs whatever happens to be in the working directory under
a trusted name. A name containing `/` is a path, and used as given.

```quiver
%exec.run ["/bin/sh", %list{ "-c", "echo direct" }] ~> =(stdout: ~) ~> Str[~]   //= "direct\n"
```

## The working directory and environment

`cwd:` sets the child's working directory. `env:` changes its environment — `Set[name,
value]` or `Unset[name]`, applied in order over the host's — and `clear_env: Ok` starts from
an empty one instead.

```quiver
%exec.run ["pwd", cwd: %path.parse "/"] ~> =(stdout: ~) ~> Str[~]   //= "/\n"
%exec.run [
  "sh",
  %list{ "-c", "echo \"$GREETING, ${HOME:-nobody}\"" },
  env: %list{ Set["GREETING", "hi"], Unset["HOME"] },
]
~> =(stdout: ~) ~> Str[~]                                            //= "hi, nobody\n"
%exec.run ["/usr/bin/env", env: %list{ Set["ONLY", "this"] }, clear_env: Ok]
~> =(stdout: ~) ~> Str[~]                                            //= "ONLY=this\n"
```

An argument or variable cannot hold a NUL byte, which the operating system has no way to
pass; one that does is a runtime error.

```quiver
%exec.run ["echo", %list{ Str[<6100>] }]   //! cannot contain a NUL byte
```

## Talking to a running program

`spawn` answers `[child, stdin, stdout, stderr]`. The pipes are ordinary byte resources:
`%io.write` sends to the program's stdin, and `%io.close` closes it, which the program reads as
the end of its input. Its stdout and stderr are read by selecting on them, and
`%io.read_both` reads the two to their ends together.

```quiver
%exec.spawn ["sort"] ~> =(child: c, stdin: i, stdout: o, stderr: e)
%io.write [i, "pear\napple\n" ~> .0]          //= Ok
%io.close i                                    //= Ok
%io.read_both [o, e] ~> =[~, _] ~> Str[~]      //= "apple\npear\n"
```

Read both together, rather than one after the other: a program that fills one pipe while the
other is being awaited stops until it is drained, and would wait forever.

The child is a select source too, ending — once — with how the program ended. A later select
answers the same end again.

```quiver
%exec.spawn ["sh", %list{ "-c", "exit 3" }] ~> =(child: c)
![c] ~> =Exited[source: _, code: ~]       //= 3
![c] ~> =Exited[source: _, code: ~]       //= 3
```

A select races the program against anything else — its output, the mailbox, a timeout:

```quiver
%exec.spawn ["sleep", %list{ "10" }] ~> =(child: c)
![c, 50] ~> :timeout<'int>                //= 50
```

## Signals

`kill` sends a signal, `Term` by default, to the child and to everything it started.

```quiver
%exec.spawn ["sleep", %list{ "10" }] ~> =(child: c)
%exec.kill [c]                                    //= Ok
![c] ~> =Signalled[source: _, signal: ~]          //= 15
```

## Ownership

A child belongs to the process that spawned it, like any resource, and moves when it is sent
or captured by a spawn. When its owner ends — or `%io.close` closes it — the program is
killed along with everything it started, and reaped. Stopping the host does the same for
every child it has. A host that dies abruptly (a crash, a `SIGKILL`) still takes its children
with it, though not anything they started in turn.

```quiver
reader = @[] {
  %exec.spawn ["sh", %list{ "-c", "echo from a child" }] ~> =(stdout: o)
  %io.read_all o
} []
!reader ~> Str[~]                         //= "from a child\n"
```

On a shared server (`quiv server`), a child runs on the server's host, with the server's
working directory and environment unless `cwd:` and `env:` say otherwise.
