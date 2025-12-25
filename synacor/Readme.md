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

- To get the address in the ROM from disassembly code you just need to take
  the address right before the opcode (because increment of IP is done in decode
  step) and multiply it by 2 (because a word in memory is 2 bytes). For example,
  if you have this code:
```
 ...
 267   │ 001e4| HALT
 268   │ 001e7| JT 0 0x432
 269   │ 001ea| JF 1 0x432
 270   │ 001ed| JT 1 0x1ef
 271   │ 001ef| JUMP 0x432
 272   │ 001f2| JF 0 0x1f4
 273   │ 001f4| JUMP 0x432
 274   │ 001f7| JT 32768 0x445
 ...
```
- You can get the position of `JT 32768 0x445` that is `0x274` by multiplying
  `0x273` (the address of the previous instruction) by 2. So in the ROM, the
  code will be at offset `0x4E6`
