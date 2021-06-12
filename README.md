# Project Elf

## Oak

Printing pipes (`<|` and `|>`) from `ElAst` with `<@` and `@>` to avoid dumping indention complexity.

## Elf Driver Design

### Command-line Arguments

```bash
elf [options] <elm-proj-path>

Options:

    Intermediate representation: dump to file named after its src; default to no action
    --dump-path <path>        alternative dump destination; default to <proj-root>/.elves
    --stdout                  explictly dump designated steps to stdout
    --layout                  dump layout-insensitive representation
    --ast                     dump Ast
    --resolved                dump resolved Ast
    --drown                   dump all possible info

    Warnings and errors:
    --report                  generate a .md report
    --skip <pass>             skip certain analyzer checks
        Pass:
        "paren-depth"         parentheses depth check (default to 6)
        "code-len"            code length check at module, func and lambda level
    --suppress                suppress certain check via comment (experiental)
    --perfect                 strict mode: ignoring all suppresses and skips; ideal time killer

Elm-proj-path: the elm project root, containing a src/ folder.
```
