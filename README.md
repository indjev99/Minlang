# Minlang
A small strongly typed imperative programming language.

Semantics:
- All types are represented as 64 bit values.
- Function arguments are passed by value and are computed from left to right.

Lexing:
- Start single line comments with `#`
- Start nested multi line comments with `#>` and end them with `<#` 

Grammar:

```
PROGRAM ::=
    (VAR_DEF | FUN_DEF)*

VAR_DEF ::=
    var VAL_TYPE NAME = EXPR;

FUN_DEF ::=
    fun FUN_TYPE NAME "(" (NAME (, NAME)*) ")" = BODY end

BODY ::=
    (VAR_DEF | STMT)*

STMT ::=
    EXPR;
  | ADDR_EXPR = EXPR;
  | return EXPR;
  | if EXPR then BODY (elif EXPR then BODY)* (else BODY) end
  | while EXPR do BODY end

EXPR ::=
    ADDR_EXPR
  | NUMBER
  | ADDR_EXPR "(" (EXPR (, EXPR)*) ")"
  | EXPR BIN_OP EXPR
  | UN_OP EXPR
  | &ADDR_EXPR
  | "(" EXPR ")"

ADDR_EXPR ::=
    NAME
  | EXPR[EXPR]
  | *EXPR
  | "(" ADDR_EXPR ")"

TYPE ::=
    VAL_TYPE | FUN_TYPE

VAL_TYPE ::=
    int
  | wildcardPtr
  | TYPE*

FUN_TYPE ::=
    TYPE "(" (TYPE (, TYPE)*) ")"
```

Subscripting and the oparators have the usual precedences.

Primive variables and functions:
- `nullptr()`
- `alloc(n)`
- `free(ptr)`
- `read()`
- `print(x)`
- `flush()`
- `exit(code)`

Future plans:
- Imports
- Custom struct types
- Arrays
- Named arguments
- Other 64 bit primitive types
- Non 64 bit primitive types (requires handling alignment)
