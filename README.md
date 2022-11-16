# Minlang
A small imperative programming language.

Semantics:
- All values are signed 64 bit intergers that can also be interpreted as pointers.
- Function arguments are passed by value and are computed from left to right.

Lexing:
- Start single line comments with `#`
- Start nested multi line comments with `#>` and end them with `<#` 

Grammar:

```
PROGRAM ::= (VAR_DEF | FUN_DEF)*

VAR_DEF ::= var NAME = EXPR;

FUN_DEF ::= fun NAME "(" (NAME (, NAME)*) ")" = BODY end

BODY ::= (VAR_DEF | STMT)*

STMT ::=
    EXPR;
  | ADDR_EXPR = EXPR;
  | return EXPR;
  | if EXPR then BODY (elif EXPR then BODY)* (else BODY) end
  | while EXPR do BODY end

EXPR ::=
    ADDR_EXPR
  | NUMBER
  | NAME "(" (EXPR (, EXPR)*) ")"
  | EXPR BIN_OP EXPR
  | UN_OP EXPR
  | &ADDR_EXPR
  | "(" EXPR ")"

ADDR_EXPR ::=
    NAME
  | EXPR[EXPR]
  | *EXPR
  | "(" ADDR_EXPR ")"
```

Subscripting and the oparators have the usual precedences.

Primive functions:
- `alloc(n)`
- `free(ptr)`
- `read()`
- `print(x)`
- `flush()`
- `exit(code)`

Future plans:
- Imports
- Simple types: int64, pointers
- Custom struct types
- Arrays
- Function pointers
- Named arguments
- Other 64 bit primitive types
- Non 64 bit primitive types (requires handling alignment)
