# Overview

- On youtube we saw a very interesting projet so let's try it...
- [Live coding a ray tracer in OCaml](https://www.youtube.com/watch?v=D_esyWms6zY&list=PL844gPdJQcO3_AuwU6HljB4suqY-wWjOO&index=2&pp=iAQB)
- [Ray tracing in one week-end](https://raytracing.github.io/books/RayTracingInOneWeekend.html)

# The journey for ray tracing...

## Let's begin by the hello world

```sh
# Create a directory
mkdir rays
cd rays
# Create a new switch for this project
opam switch create ./ 5.1.0
# Update the environment
eval $(opam env)
# install dune to manage the project and ppx_expect used for testing
opam install ppx_expect dune -y
# Init the project (one level up and generate the project into rays/)
cd ../
dune init proj rays --inline-tests --ppx ppx_expect
# Check that everything is working
dune build
dune test
dune exec rays
```
## Add test

- Modify *test/* by renaming `rays.ml` into `rays_test.ml`
- Update dune file to [use ppx_expect](https://dune.readthedocs.io/en/stable/tests.html)
- Add an example in `rays_test`
- Run `dune test`

## Add first function in our library

- Create a module *Image* in `lib/rays.ml` with an *hello* function.
- From `bin/main.ml` open Rays and call the *hello*
- Modify the test to validate function within the lib
