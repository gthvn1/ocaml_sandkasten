# Overview

- On youtube we saw a very interesting projet and so we try to follow it...
- [Live coding a ray tracer in OCaml](https://www.youtube.com/watch?v=D_esyWms6zY&list=PL844gPdJQcO3_AuwU6HljB4suqY-wWjOO&index=2&pp=iAQB)

# Let's begin

```sh
# Create a directory
mkdir rays
cd rays
# Create a new switch for this project
opam switch create ./ 5.1.0
# Update the environment
eval $(opam env)
# install dune to manage the project
opam install dune -y
# Init the project (one level up and generate the project into rays/)
cd ../
dune init proj rays --inline-tests --ppx ppx_expect
```

