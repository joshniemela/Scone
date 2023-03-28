# PlaceholderLang
This is a repo for the designing of a weird metaprogramming "language" that allows any arbitrary input from any other programming language.  

# `:` denotes an escape into the internal lisp-like language, 

# Grammar (WIP)
`@fun_name args...;kwargs...` declares a macro call  
`@fun_name args...; kwargs... block` declares a macro call with a block  

`kwarg` is either a symbol or a key-value pair separated by `=>`, if it a lone key, it is assumed to mean `key => true` or `!key => false`

`#` is a comment and has a corresponding multiline: `#=... =#`  
`$` is a value escape and has a corresponding expression escape: `$=... =$`  
`!` is a math variable and has a corresponding math expression escape: `!=... =!` 

## Blocks
blocks behave as a list of elements separated by newline(?)  
`begin` begins a block  
`end` ends a block

### TODO:
figure out types, strings etc, how to deal with long arguments or some that contain commas.  
#### Possible string syntax: `"..."

## Alternative lispy syntax
`@fun_name args... :key value :key value...` declares a macro call  
`@fun_name args... :key value :key value... block` declares a macro call with a block  

## Alternative^2 syntax
`:fun_name kwargs... & args...` is the format for a function  
`:fun_name args...` is the format for a function  
kwargs are of type `set ({key})` or `pair (key => value)`

### POSIBLE DICT IDEA
`{}` is the empty set  
`{key}` is an implicitly true key  
`{!key}` is an implicitly false key  
`{key => value}` is a key-value pair  



### Manifesto
¤ is used as a placeholder and is to be changed to something else later.  
Strings are first class citizens, and everything is assumed implicitly to be a string unless otherwise specified.  

Functions have to be preceded by `¤` to be interpreted as a function call, otherwise it is assumed to be a string.  

Code blocks are delimited by `:=` and `=:`, and harshly escape everything inside and returns a code block type. This can be consumed by other functions, if not then it is assumed to be a placeholderlang block.

A code block with an unspecified language is assumed to be a placeholderlang block where prefixing `¤` is not required.

Functions are called with `¤fun_name kwargs... & args...`
Symbols are syntax sugar for key-value pairs, where the key is the symbol and the value is true. A preceding `!` negate the symbol. A symbol has the syntax `¤symbol` or `!¤symbol`  
`¤` is used as a placeholder and is to be changed to something else later.

