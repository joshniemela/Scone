# Syntax
Parsing a file will return an S-expression tree. This follows the same logic as any ordinary scheme or lisp parsing.
## Code mode
This is syntax unique to code mode, aka being inside of a scope preceded by `:`
`[...]` denotes a markdown block, this will effectively run a parser similar to the document parser but locally in the block and returns a list.
## Markup mode 
This is the default state of the language, anything will be interpreted as markup as long as one of the special characters: `[ ] ( ) # ' : "` are met. If they are met, the language will go into another mode. These can of course be escaped.
