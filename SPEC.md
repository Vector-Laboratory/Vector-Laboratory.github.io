# Arra Language Specification

## TODO LIST

- [ ] Type Synonyms
- [ ] `newtype`
- [ ] Finish Section Syntax
- [ ] Reconsider complex operator subtleties
- [ ] Reconsider control characters in complex operators
- [ ] Explicit Predicate Syntax
- [ ] More Number Literals
- [ ] `trace` Sublanguage
- [ ] Finish Module Language
- [ ] Clean up pattern syntax

## Syntax

The syntax used in the following document will follow the syntax of [Pest](pest.rs/) expressions, given it is the formal grammar for the parser. This document will otherwise act as an natural language specification of the grammar, and will highlight necessary subtleties contained in it.

__Note:__ This document is currently incomplete due to certain aspects of the language grammar that are not finalized. As such, this document will describe the grammar, *as is*, and will be updated as the unsettled becomes settled.

## Grammar

### Whitespace

```sml
WHITESPACE = _{ " " | "\t" | "\r" | "\n" }
```

As currently defined, the language is whitespace insensitive. This means that anywhere in the following document, in most cases, where `~` is used, it may contain any number of these characters. The special cases where `~` is interpreted differently, is when the defining block starts with `$`, or `@`. The details are described here: [Atomic Rules in Pest.](https://pest.rs/book/grammars/syntax.html#silent-and-atomic-rules)

### Comments

```sml
COMMENT = _{
             "//" ~ (!"\n" ~ ANY)*
  | !"/'-" ~ "/'" ~ (!"'/" ~ COMMENT? ~ ANY)* ~ "'/"
}
```

There are two styles of comments in Arra. Replicating exactly C style comments when they are single lined, and closely following with multiline comments with just one minor change. The reason why comments use `/' ... '/` and not `/* ... */`, is because `/*` is a valid expression in Arra read as "folding multiply". However because `'` is an iterator like `/`, and it is syntactically illegal for two iterators two appear next to each other, this form was chosen.

### Semicolons

```sml
semicolon = { ";" }
```

This is a capturing form, used to capture semicolons when they are not used as expression separators. In these cases, the number of semicolons is semantically significant.

### Top Level

```sml
top_level = _{ SOI ~ program ~ EOI }
```

Every top level expression typed into the REPL or in a file: Is a program that spans the entire input.

### Programs

```sml
program = { (expression ~ semicolon*)* ~ expression? }
```

A program is a series of expressions, separated by one or more semicolons.

### Attributes

```sml
attr = { "/'-" ~ (attr_inner ~ semicolon*)* ~ attr_inner? ~ "-'/" }

attr_inner = { constructor ~ value? }
```

Attributes use syntax similar to comments, however they follow a strict internal structure. An attribution can apply multiple different attributes to the expression it is modifying, by separating these attributes by semicolons. A attribute must be registered in the attribute store, and it is referenced by it's constructor, which receives one or no arguments.

### Expressions

```sml
expression = { expr_with_attr | expression_ }
expr_with_attr = { attr ~ expression_ }
expression_ = _{
    application_overload
  | attribute_definition
  | dict_overload
  | data_definition
  | iter_overload
  | macro_
  | op_overload
  | open_module
  | table_definition
  | trait_definition
  | vec_overload
  | (bind | value) ~ projection?
}
```

Expressions may be attributed or unattributed. Expressions can take various forms, all of which will be presented in the following sections.

### A note on application, iterator, and operator overloading!

For each set of overloads, the order in which the overloads are defined is important. The compiler will match the pattern of types based on the order that the overloads are defined. This means that when the compiler encounters an application, an iterator, or an operator: It will try in order, the relevant overloads defined.

### Type Constraints

```sml
type_constraint = {
  "[" ~ expression ~ (";" ~ expression)* ~ "]" ~ "=>"
}
```

A number of the following expressions require the ability to define a type constraint. As the language is currently defined, Arra just reuses the syntax of expressions. However as the type system matures, this syntax will change.

### Application Overloading

```sml
application_overload = {
  "apply" ~ type_constraint? ~
    pattern ~ "," ~ pattern ~ "->" ~ pattern ~ ":" ~
    program
}
```

For a full appreciation of the syntax, consider two examples:

```sml
apply [i <- m: n >= i] => Char[n], Int[m] -> Char[m]:
  prim.str_vec_proj;

apply [n >= i && m >= j] => a[n;m], ([i;j] : Int[2]) -> a:
  prim.2d_index;
```

These two uses of the apply syntax define when a string is is projected by a vector and when a 2 dimensional array is indexed by another vector of length 2.

```kdb
"HURRAY"[4;3;2;4]   // Evaluates to: "ARRA"
[[1;2];[3;4]][0;1]  // Evaluates to: 2
```

### Attribute Definitions

```sml
attribute_definition = { "attr" ~ constructor ~ ":" ~ function }
```

This is the sublanguage for defining `/'-` attributes `-'/`. The function is expected to take 2 arguments, the first argument is the argument to attribute, the second is a quoted program of the expression that is being attributed. A hypothetical attribute definition may look like:

```ocaml
attr Hyp: {[arg;x] match x [`[data .name ...] -> ...]};
```

### Dictionary Overloads

```sml
dict_overload = { "dict" ~ constructor }
```

To enable overloadable dictionaries, they need a syntax to allow that syntax to be overloaded. This is essentially "registering" with the language that a specific type that you have define, named by it's Constructor, can act as a dictionary. The compiler at method resolution time will check to make sure that constructor also defines the `IsDict` trait. If no such trait for that type exists in scope, then it raises an error. Otherwise the overloading has succeeded.

### Data Definitions

```sml
data_definition = {
    pattern_vector? ~ constructor ~ ":" ~ data_constructor_body
  | pattern_vector? ~ constructor
}

data_constructor_body = _{ struct_defn | enum_defn }

struct_defn = { "{" ~ (constructor ~ ":" ~ pattern)* ~ "}" }

enum_defn = { "|"? ~ pattern ~ ("|" ~ pattern)* }
```

Arra has a rich data definition language similar to other statically typed functional programming languages. As such, you can define product and sum types. A data definition always has the form of:

```sml
data Constructor: ...body...;
```

Optionally a data definition can take generic type arguments, for example:

```sml
data [a;b] Constructor: ...body...;
```

A data definition does not require a data definition body, these bodiless forms form Phantom Types:

```sml
data Constructor;
```

A data definition body is either a `struct` or an `enum`, this mirrors product and sum types respectively.

A `struct` is defined using curly brackets and may have zero or more rows, and resembles the form:

```sml
data Constructor: { One : Int; Two : String };
```

An enum is defined as a series of type patterns separated by a pipe `|`, the first pipe is optional:

```sml
data Constructor: One Int | Two String;
```

__Notably!__ The application semantics for *instances* of types does not follow the LTR ordering of type *application* and follows RTL semantics, like binding. This is discussed further in the [rationale](./RATIONALE.md).

__TODO:__ Type synonyms and `newtype`-like definitions.

### Iterator Overloads

```sml
iter_overload = {
  "iter" ~ type_constraint? ~ iter ~
    pattern ~ "," ~ pattern ~ ("," ~ pattern)? ~
      "->" ~ pattern ~ ":" ~
    program
}
```

Iterator overloads are similar to the other overloading sublanguages. This sublanguage differs in one large respect to all the other sublanguages, in that it has an optional type pattern. Here is an example of it's use:

```sml
iter 'fold a[],    (b -> a -> b) -> b: prim.vec_fold_default;
iter 'fold a[], b, (b -> a -> b) -> b: prim.vec_fold;

iter 'map a[n], (a -> b) -> b[n]: prim.vec_map;
```

These examples define mapping and folding, similar to most other languages.

Furthermore, these instances of overloading can depend on the value of the arguments:

```sml
iter 'rank a[n;m], (1 : Int), (a[] -> a) -> a[n]: prim.rank1;
iter 'rank a[n;m], (2 : Int), (a[] -> a) -> a[m]: prim.rank2;
```

This is used to capture the semantics of rank conjunctions in J:

```j
   i. 2 3
0 1 2
3 4 5
   +/"1 i. 2 3
3 12
   +/"2 i. 2 3
3 5 7
```

In Arra:

```kdb
[2;3] iota 'rank 1 (/+)  // [3;12]
[2;3] iota 'rank 2 (/+)  // [3;5;7]
```

This semantics will likely require each overload that depends on a runtime value: To either be statically determined, or if it cannot be determined, then some form of runtime dispatch will be required, likely by branching into monomorphized code.

### Macros

```sml
macro_ = { macro_name ~ "[" ~ macro_branch+ ~ "]" }
macro_name = { LETTER+ }

macro_branch = { quoted_program ~ "->" ~ macro_rhs ~ ";"* }
macro_rhs = {
    quoted_program
  | program ~ ";"+ ~ quoted_program
}
```

Macros are formed by the `macro` keyword. They have a name so they may be explicitly applied to a form, although this is the uncommon case. Macros contain at least one branch. Each branch has a left hand side and a right hand side. The left hand side of a macro is always a pattern in the form of a quoted program, and the right hand side is either a literal quoted program, or some program that generates a quoted program.

### Operator Overloading

```sml
op_overload = {
  "op" ~ type_constraint? ~ op ~
    pattern ~ "," ~ pattern ~ "->" ~ pattern ~ ":" ~
    program
}
```

Similar to application and iterator overloading, actually the least interesting of the overloading sublanguages.

### Modules

```sml
open_module = { "open" ~ constructor ~ ("." ~ constructor)* }
```

Currently the module system and it's syntax are extremely basic. This will improve when the rest of the language matures.

### Table Definitions

```sml
table_definition = { "table" ~ constructor ~ "{" ~ bind+ ~ "}" }
```

Tables are defined with a very familiar syntax. When row types are finalized, this syntax is sure to change. See the [rationale](./RATIONALE.md) for more details.

### Trait Definitions

```sml
trait_definition = {
  "trait" ~ type_constraint? ~
    pattern_vector? ~ constructor ~ "[" ~
      bind+ ~
    "]"
}
```

Traits are very similar semantically to traits in Rust and Typeclasses in Haskell. The syntax is also similar.

### Vector Overloading

```sml
vec_overload = { "vec" ~ constructor }
```

### Variable Binding and Destructuring

```sml
bind = { pattern ~ ":" ~ expression }
```

A binding has a pattern on it's left hand side and an expression on it's right hand side. A pattern may be a named variable, a tuple, a vector, or any number of patterns defined below, and the value on the right will be destructurally bound to the pattern on the left, following the expected rules.

### Patterns

```sml
pattern = { pattern_value ~ pattern_projection? }

pattern_value = _{
    tuple_pattern
  | "(" ~ pattern ~ ")"
  | typed_pattern
  | variable_
  | constructor ~ pattern?
  | number_
  | character_
  | string_
  | symbol_
  | pattern_vector
  | dictionary_pattern
}

pattern_projection = {
    array_type_projection
  | function_projection
}

array_type_projection = {
  "[" ~ ("_" | (expression ~ (";" ~ expression)*))? ~ "]"
}

function_projection = { "->" ~ pattern }

tuple_pattern = { "(" ~ (pattern ~ ("," ~ pattern)*)? ~ ")" }

typed_pattern = { "(" ~ pattern ~ ":" ~ pattern ~ ")" }

dictionary_pattern = {
  "{" ~ (pattern ~ ":" ~ pattern)* ~ "..."? ~ "}"
}
```

Arra at it's core a pattern language. All of these definitions specify the various uses of types in the language, as well as the patterns that for destructuring. This will likely be cleaned up.

### Values

```sml
value = _{
    tuple
  | parenthesized_expression
  | projection_value
  |       iter_value
  |   operator_value
  |     iter_section
  | operator_section
  | parenthesized_program
  |        quoted_program
  |      unquoted_program
  | if_
  | match_
  | function
  | dictionary
  | vector
  | vector_projection
  | string_
  | symbol_
  | character_
  | number_
  | variable_
  | constructor ~ value?
}
```

There are a number of special forms that comprise a value, those are discussed in the following section.

Otherwise a value can be an `if` or `match` expression, function, dictionary, a vector, a vector projection, string, symbol, character, number, variable, or constructor optionally applied to a value.

### Special Forms

```sml
                   tuple = { "(" ~ (value ~ ("," ~ value)*)? ~ ")" }
parenthesized_expression = { "(" ~ expression ~ ")" }

projection_value = { "(" ~ projection ~ ")" }
      iter_value = { "(" ~ iter ~ ")" }
  operator_value = { "(" ~ operator ~ ")" }

    iter_section = { "(" ~ value ~ iter ~ ")" }
operator_section = { "(" ~ value ~ operator ~ ")" }

parenthesized_program = { "(" ~ program ~ ")" }
       quoted_program = { "`[" ~ program ~ "]" }
     unquoted_program = { ".(" ~ program ~ ")" }
```

First, the syntax for tuples, very standard.

Otherwise, there are a number of special forms in Arra, mostly comprised of the various syntactical elements typically juxtaposed, instead stated alone. This is performed by parenthesizing the expression, the special forms that follow this pattern are: Expressions themselves, projections, iterators, operators, iterator sections, operator sections, and programs.

The remaining special forms are for quoting and unquoting programs: Used exclusively by the macro system.

__TODO:__ Section syntax needs to be finished.

### Projections

```sml
projection = _{
    iter_w_value
  | iter_default
  | op_projection
  | application
}

iter_w_value  = { iter     ~ value ~ iter_arg ~ projection? }
iter_default  = { iter             ~ iter_arg ~ projection? }
op_projection = { operator ~ value            ~ projection? }
application   = {            value            ~ projection? }

iter_arg = _{
    operator
  | variable_
  | projection_value
  | parenthesized_program
  | function
}
```

This is the core of the grammar. When reading this section, keep in mind the relevant definition of expressions, in a simplified representation here:

```sml
expr = { value ~ projection? }
```

Projections can only appear juxtaposed by values, or otherwise in it's special form.

Taking note of this, there are 4 main forms of projections: Two similar forms for iterators, one form for operators, and the last for application.

Iterators are limited in the arguments it can take. An iterator can take either 2 or 3 arguments, and the right hand side argument it takes is limited to operators, variables, specially formed projections, parenthesized programs, or anonymous functions.

In addition, the optional argument to an iterator can take any value.

__To note:__ Applications are just a projection of a value by a value, and applications are formed though *juxtaposition* of these values. Furthermore, the application of a function to a function with congruent types, is just it's composition: Making this grammar [quasi-concatenative](https://en.wikipedia.org/wiki/Concatenative_programming_language).

### If and Match

```sml
if_ = {
  "if" ~ "[" ~
    expression ~ ";" ~
    expression ~ (";" ~ expression?)? ~
  "]"
}

match_ = { "match" ~ value ~ "[" ~  match_branch+ "]" }

match_branch = { pattern ~ "->" ~ program ~ ";"* }
```

Here are the cores of the pattern matching sublanguages in Arra. When compared to pattern matching in other languages, it is very similar.

### Functions, Sections, and Predicates

```sml
function = { "{" ~ pattern_vector? ~ program ~ "}" }
predicate = { "`{" ~ "}" }
```

Anonymous functions work very similarly to the APL family, their syntax is directly inspired by the syntax invented by Arthur Whitney.

An anonymous function may have up to 3 implicit arguments, `x`, `y`, and `z`. The arity of the function is inferred from their use in the body of the function. Otherwise an arbitrary number of arguments can be explicitly defined for the function in the following form: `{[a;b;c;d]a*b-c%d}`.

__TODO:__ Finish explicit predicate syntax. Support the Iverson bracket.

### Dictionaries and Overloading

```sml
dictionary = ${ dict_header ~ "{" ~ (bind ~ ";"*)* ~ "}" }
dict_header = @{ ":" ~ constructor ~ ":" }
```

Dictionaries in Arra currently use the form: `:Hash:{x:1;y:2}`. The dictionary header is used to overload the type of dictionary, for instance you can use a b-tree backed map: `:Btree:{x:1;y:2}`

### Vectors, Projections, Pattern Vectors, and Overloaded Vectors

```sml
pattern_vector = { "[" ~    (pattern ~ (";" ~    pattern)*)? ~ "]" }
vector         = { "[" ~ (expression ~ (";" ~ expression)*)? ~ "]" }

vector_projection = { "[" ~ (expression ~ semicolon*)* ~ "]" }

overloaded_vector = { ":" ~ constructor ~ ":" ~ vector }
```

### Operators

```sml
operator = ${ (complex_operator | simple_operator) }

simple_operator = @{ op+ }

complex_operator =
    @{ "," ~ op* ~ (op_inner ~ (op* ~ op_inner)*)* ~ op*  }

op_inner = { LETTER | SYMBOL | NUMBER }

iter = ${ !character_ ~ iterator | "'" | "/" | "\\" }

iterator = @{ "'" ~  (op | op_inner | "/" | "\\")+ }

op = {
    "~" | "!" | "@" | "#" | "$" | "%" | "^" | "&" | "*" | "-" |
    "_" | "=" | "+" | "|" | "," | "<" | ">" | "." | "?" |
    (!"`" ~ SYMBOL)
}
```

Operator syntax is arguably the most subtle parts of Arra. I wanted to enable arbitrary user defined operators, and for the language to be able to be parsed simply. So there is a minor compromise to enable both of these aims. If you the reader invents a less subtle operator syntax that satisfies both of these aims, certainly inform me.

Explained through example, the operator syntax of Arra looks like this:

```kdb
a+b
```

Parses to:

```lisp
(+ a b)
```

Likewise we have:

```kdb
a++b
```

Which, as expected, parses to:

```lisp
(++ a b)
```

A __simple operator__ is just: The juxtaposition of any number of operator characters or symbols, uninterrupted by spaces.

An interesting special case of operators, as they interact with numbers, is the dash character: `-`. As the grammar is currently defined, this isn't much of a problem, if you want to negate a negative value, it looks like: `-1-`.

A __complex operator__ is just: `,` followed by zero or more simple op characters, letters, numbers, or symbols, terminated by a space.

The last part of the prior sentence is where the subtlety comes in. Having to terminate all complex operators with a space can lead to some subtleties in how an expression is parsed. Consider these two Arra expressions:

```kdb
3!/, [] // Parses as expected

3!/,[]  // ,[] is entirely parsed as a single complex operator
```

In other words, these are the resulting parse trees:

```lisp
(/ [] , (! 3))

(/ ,[] (! 3))
```

So this subtlety will have to be held in mind when using complex operators.

__HELP WANTED:__ This should not be much of a problem. A custom error message for this case can be made highlighting this subtlety. Although ideally I want both of these expressions to parse identically. Any suggestions would be highly appreciated.

### Control Characters

__TODO:__ This is an explicit definition of all of the control characters used in Arra. I think it might be possible to add these characters as legal inside *complex* operators. I have tried some versions of trying to integrate these characters, yet none I've found has been quite satisfactory.

The obvious integration in this context is simply to add `ctrl` to `op_inner`, however I do remember that it caused some undesirable parses, I will have to investigate the situation again, soon.

```sml
ctrl = { "(" | ")" | "[" | "{" | "]" | "}" | ":" | ";" }
```

### Symbols

```sml
symbol_ = ${ sym_start ~ sym_inner }
sym_inner = @{ sym_* }
sym_start = _{ "`" }
sym_ = { !(WHITESPACE | "`" | ";" ) ~ ANY }
```

Symbols have very similar syntax to K, they start with a \` and end with either a semicolon, whitespace, another \`. Otherwise they may contain any other characters.

### Characters, Strings, and Interpolation

```sml
string_ = ${ "\"" ~ str_inner* ~ "\"" }
str_inner = @{
    "\\\""
  | "{{" | "}}"
  | str_interp
  | chr_+
  | str_interp_err
}
str_interp = !{ "{" ~ program ~ "}" }
str_interp_err = !{ "{" | "}" }

character_ = ${ "'" ~ chr_inner ~ "'" }
chr_inner = @{ "\\" ~ "'" | chr_ }
chr_ = {
  !("\"" | "\\" | "{" | "}") ~
    ANY
  | "\\" ~ ("\\" | "/" | "n" | "t" )
  | "\\" ~ ( "u" ~ ASCII_HEX_DIGIT{4})
}
```

Strings and characters have familiar syntax. The one minor novelty is expression interpolation.

### Numbers

```sml
number_ = @{
  "-"? ~ ("0" | ASCII_NONZERO_DIGIT~ASCII_DIGIT*) ~
  "."? ~ ASCII_DIGIT* ~ (^"e" ~ ("+" | "-")? ~ ASCII_DIGIT+)?
}
```

Very standard implementation of parsing floating point numbers.

__TODO:__ Different literals for different types of numbers.

### Keywords

```sml
keyword = {
    "apply"
  | "attr"
  | "data"
  | "dict"
  | "if"
  | "iter"
  | "macro"
  | "match"
  | "op"
  | "open"
  | "table"
  | "trace"
  | "trait"
  | "vec"
}
```

Since Arra is defining a number of sublanguages, for the sake of better error messages and ease parsing: All sublanguages in Arra are defined by introducing that sublanguage with a leading keyword.

### Variables

```sml
variable_ = @{ !keyword ~ ("_" | LOWERCASE_LETTER) ~ var_* }
var_ = { "_" | "." | LETTER }
```

All variables in arra start with a lowercase letter.

### Type Constructors

```sml
constructor = @{ (UPPERCASE_LETTER | TITLECASE_LETTER) ~ constr_* }
constr_ = { LETTER }
```

All constructors start with an uppercase or titlecase letter.
