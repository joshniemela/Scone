# Syntax
Parsing a file will return an S-expression tree. This follows the same logic as any ordinary scheme or lisp parsing.
## Markup blocks
Surrounding something with `[ ... ]` will harshly escape it from the language and treat it as an atomic object where `:` can be used to escape the block temporarily to evaluate an SExpr.
