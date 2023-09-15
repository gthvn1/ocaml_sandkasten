# Starting the project

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
