# Setup

- We create a switch at the toplevel of the sandkasten if it doesn't
  already exist: `opam switch create ./`
- Upate env: `eval $(opam env)`
- Install Lwt and other intresting stuff if it is not already installed:
  `opam install dune utop lwt base-unix`

# utop

- We will follow the tutorial from [mirage.io](https://mirage.io/docs/tutorial-lwt)
- Utop is cool for testing. Just need to load *Lwt*:
```ocaml
utop # #use "topfing";;
utop # #require "lwt.unix";;
```
