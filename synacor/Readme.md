- The [Synacor Challenge](https://web.archive.org/web/20230206005149/https://challenge.synacor.com/) was an online programming puzzle created by [Eric Wastl](https://x.com/ericwastl), the mind behind [Advent of Code](https://adventofcode.com).
- run: `dune exec synacore -- challenge.bin`
- There is a simple debugger:
```
❯ dune exec synacor -- --help
Entering directory '/home/gthouvenin/devel/ocaml/ocaml_sandkasten'
Leaving directory '/home/gthouvenin/devel/ocaml/ocaml_sandkasten'
USAGE: synacor [--break <addr>] [--debug] <rom>
❯ dune exec synacor -- roms/challenge.bin
Entering directory '/home/gthouvenin/devel/ocaml/ocaml_sandkasten'
Leaving directory '/home/gthouvenin/devel/ocaml/ocaml_sandkasten'
Welcome to the Synacor Challenge!
Please record your progress by putting codes like
this one into the challenge website: gmpJPiyErvJM

Executing self-test...

Fatal error: exception Failure("ERROR: unknown instruction to execute at 0x140)")

❯ dune exec synacor -- roms/challenge.bin --break 0x140
Entering directory '/home/gthouvenin/devel/ocaml/ocaml_sandkasten'
Leaving directory '/home/gthouvenin/devel/ocaml/ocaml_sandkasten'
Welcome to the Synacor Challenge!
Please record your progress by putting codes like
this one into the challenge website: gmpJPiyErvJM

Executing self-test...

[s]tep [p]rint [c]ontinue [q]uit
> p
   ----- MEM -----
   Mem[315]:0x002E
   Mem[316]:0x0013
   Mem[317]:0x000A
   Mem[318]:0x0013
   Mem[319]:0x000A
=> Mem[320]:0x0006
   Mem[321]:0x015B
   Mem[322]:0x0013
   Mem[323]:0x006A
   Mem[324]:0x0013
   Mem[325]:0x006D

[s]tep [p]rint [c]ontinue [q]uit
> q
```
