# PlaceholderLang
This is a repo for the designing of a weird metaprogramming "language" that allows any arbitrary input from any other programming language.  

## Grammar (WIP)
`@fun_name args...; kwargs...` declares a macro call  
`@fun_name args...; kwargs... block` declares a macro call with a block  

`kwarg` is either a symbol or a key-value pair separated by `=>`, if it a lone key, it is assumed to mean `key => true` or `!key => false`

`#` is a comment and has a corresponding multiline: `#=... =#`  
`$` is a value escape and has a corresponding expression escape: `$=... =$`  
`!` is a math variable and has a corresponding math expression escape: `!=... =!`

To escape one of `#$!`, write the chacter twice:
- \## compiles to #
- $$ compiles to $
- !! compiles to !

## Blocks
blocks behave as a list of elements separated by newline(?)  
`begin` begins a block  
`end` ends a block

## Semantics
Everything in the file is (or parses to) one of the following:
- Raw text
- `#` A comment
- `@` A macro call
- `$` Interpolation

### Values
- Values can be defined in the `@preamble` macro or inline.
- Values can be interpolated.
- Values provide formatting.

### Macros
- A macro returns a value.
- The compiler will have a minimal(?) set of functionally complete macros that can generate any PDF.

### Complementary Languages
- The compiler can be hooked into by *any* other programming language.
- A complementary language can attach values to interpolation keys by providing `key => value` pairs.
- A complementary language can override macros by providing an alternate implementation.
- If a macro is unknown by the compiler, it will be given a placeholder value until a complementary language provides an implementation for it.
- Values interpolated from or provided by a complementary language must adhere to a standard format.

### Inline code
- Inline code will be parsed into its own file.
- The compiler will run the appropriate program to run that code and retrieve it's return value(s).
- The return value(s) will be interpolated into their respective locations.
- Any number of complementary languages can be inlined in the same document.

## Design Goals
- Minimal markup language.
- Compiles to PDF.
- Inline LaTeX, Markdown, and HTML.
- Computation in *any* programming language via hooks and inline code.

## TODO:
figure out types, strings etc, how to deal with long arguments or some that contain commas.  
#### Possible string syntax: `"..."

## Alternative lispy syntax
`@fun_name args... :key value :key value...` declares a macro call  
`@fun_name args... :key value :key value... block` declares a macro call with a block  
