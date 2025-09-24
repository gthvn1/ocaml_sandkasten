This is my personal OCaml playground. A chaotic, ever-growing dune workspace where I dump all my toy projects,
weird experiments, and half-baked ideas. Nothing here is polished, documented, or meant for public consumption.
It’s just a sandbox for me to break things, test wild hypotheses, and occasionally stumble upon something cool.

(You’ve been warned.) 🧪🔥

# Hints
- Check for correctness: `opam lint .`
- Build all targets: `dune build`
- Build a specific project: `dune build crepe`
- Run a project: `dune exec crepe`
- Launch a REPL for a project: `dune utop crepe`
- Browse module documentation in your terminal: `ocp-browser`
- Look the source: `batcat $(ocamlfind query unix)/unix.mli`

# Useful Resources
- [OCaml](https://ocaml.org/)
- [OCaml Programming: Correct + Efficient + Beautiful](https://cs3110.github.io/textbook/cover.html)
- [Dune file reference](https://dune.readthedocs.io/en/stable/reference/dune/)
- [Dune-project file reference](https://dune.readthedocs.io/en/stable/reference/dune-project/)
- [Opam](https://opam.ocaml.org/)
