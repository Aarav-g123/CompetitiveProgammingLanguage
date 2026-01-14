# CompetitiveProgammingLanguage

A minimal cp-lang -> C toy pipeline. Currently only accepts the snippet:

```cp
fun main() { return 0; }
```

## Build

```bash
cabal build
```

## Run

```bash
cabal run cp-lang -- -i test.cp -o out.c
cat out.c
```

`test.cp` already contains the supported program. The compiler will write `out.c` and print the output 
path. 

## TODO
- Extend lexer/parser/typechecker beyond the single hardcoded program
- Flesh out runtime and safety checks
- Add real tests/CI once the language surface grows