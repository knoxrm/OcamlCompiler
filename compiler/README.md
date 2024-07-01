# Requirements
Please install ocaml, opam and dune to run this. 
After installing opam, run `opam install base`

# Usage
## Building the project
```bash
$ dune build
```
## Running the parser
```bash
$ dune exec compiler
```

# TODO after this
-[] Write an evaluator
-[] Extend the types of tokens that can be parsed and lexed. 

# Limitations
- Only types that can be parsed right now are int, void and bool. I plan to extend then in the future with types like float, double. 
- Arrays are still not able to be parsed, but implementing them should be trivial. Same with maps I think
