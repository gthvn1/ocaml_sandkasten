We give three example of DSL from simple to interesting.
  - Arithmetic_dsl
  - Build_dsl
  - Html_dsl

And the common pattern of DSL is two things:
  - A syntax to describe the specific needs we have (arithmetic, build or html)
  - An evaluator that produces the output.

Note that the syntax, like with the HTML example, often comes with
helpers/constructors/operators to make it ergonomic.

To run example: `dune exec dsl_tuto`
