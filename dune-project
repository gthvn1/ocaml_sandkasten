(lang dune 3.20)

(generate_opam_files true)

;; common metadata applied to all packages unless overridden

(license MIT)

(maintainers "gthvn1@gmail.com")

(authors "gthvn1@gmail.com")

(homepage "https://github.com/gthvn1/ocaml_sandkasten")

(bug_reports "https://github.com/gthvn1/ocaml_sandkasten/issues")

(package
 (name parse_xapi_db)
 (synopsis "Parse xapi db and generate a static HTML to explore it"))

(package
 (name aoc2025)
 (synopsis "Advent of Code 2025"))

(package
 (name hello)
 (synopsis "An OCaml sandbox: chaotic experiments, for fun and learning"))

(package
 (name codekata)
 (synopsis "Practice with CodeKata an interactive platform for programming"))

(package
 (name ocaml_exercises)
 (synopsis
  "Exercices inspired by Ninety-Nine Lisp Problems which in turn was based on Prolog problem list")
 (depends ounit2))

(package
 (name crepe)
 (synopsis
  "A flat, simple, and served hot server. Intended for learning and experimentation"))

(package
 (name maze)
 (synopsis "A robot that pushes boxes in a Maze. Nothing more")
 (depends graphics))

(package
 (name roguelike)
 (synopsis "A simple roguelike game to have fun when programming")
 (depends notty random))
