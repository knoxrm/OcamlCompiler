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
## Testing the IR codegen
```bash
$ dune runtest
```

# TODO after this
-[] Figuring out a code gen 
-[] Figure out how to write how to write basic passes
-[] Extend the types of tokens that can be parsed and lexed. 

# Limitations
- Only types that can be parsed right now are int, void and bool. I plan to extend then in the future with types like float, double. 
