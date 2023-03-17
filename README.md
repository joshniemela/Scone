# PlaceholderLang
This is a repo for the designing of a weird metaprogramming "language" that allows any arbitrary input from any other programming language.  

# Grammar (WIP)
`@fun_name args...; kwargs...` declares a macro call  
`@fun_name args...; kwargs... block` declares a macro call with a block  
## Alternative lispy syntax
`@fun_name args... :key value :key value...` declares a macro call  
`@fun_name args... :key value :key value... block` declares a macro call with a block  

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
