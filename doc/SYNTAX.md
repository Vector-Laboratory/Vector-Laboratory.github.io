# Arra Syntax

## Scope

This document specifies Arra's surface syntax using a notation-agnostic EBNF.

Decisions not yet made are marked **⚑ open**.

---

## Notation

This document uses two notations, both rendered as `ebnf` code blocks:

**Lexical grammar** (§1): regex-style character classes (`[a-z]`, `[0-9]`),
PEG negative lookahead (`!`), and `ANY` wildcard for lexical rules where
EBNF character enumeration would be unreadably verbose.

**Syntactic grammar** (§2 onward): extended EBNF. Rules use `=`. Terminals
are `'quoted'` strings or `UPPER_CASE` names. Nonterminals are `lower_case`.
Postfix: `x?` (zero or one), `x*` (zero or more), `x+` (one or more).
Grouping: `(x y)`. Alternatives: `|`. Comments: `(* ... *)`.

---

## 1. Lexical Structure

### 1.1 Identifiers

```ebnf
lower  = [a-z][a-z0-9_]*
upper  = [A-Z][a-zA-Z0-9_]*
tick   = '\'' lower          (* iterator/modifier names: 'map, 'fold, 'rank *)
```

`lower` names are values, functions, and type variables. `upper` names are
types, constructors, and modules. `tick` names are the syntactic category for
iterator/modifier hooks — they are `lower` names prefixed with `'`.

The parser makes no semantic distinction between `op` tokens and `tick` tokens;
both are "symbolic forms". The type checker determines the role of each
(operator vs. modifier) via kind-first dispatch. See SEMANTICS.md §3.1.

**Globally reserved identifiers**: in addition to definition-level keywords
(`type`, `alias`, `distinct`, `derived`, `effect`, `module`, `interface`,
`implementation`, `trait`, `rec`, `open`, `when`), the following identifiers
are globally reserved and may not be used as value, function, or type variable
names in any context:

| Identifier | Role |
|------------|------|
| `ctl` | non-resumable operation marker in `effect_def` and `handler_block` |
| `op` | resumable operation marker in `effect_def` and `handler_block` |
| `return` | return-value clause marker in `handler_block` |
| `Never` | ASCII alias for `⊥` (never / bottom type) |

These are not contextual keywords. They are reserved in all positions,
including inside lambdas, `let`-style bindings, and module bodies.

### 1.2 Operators

```ebnf
ascii_op_char   = '+' | '-' | '*' | '/' | '%' | '^' | '&' | '|'
                | '!' | '~' | '=' | '<' | '>' | '?' | '@' | '.'
                | '#' | '$' | '\'

unicode_op_char = any non-whitespace, non-identifier Unicode code point
                  not in the reserved list below

op_char = ascii_op_char | unicode_op_char
op      = op_char+
```

Operators are sequences of one or more `op_char`s, parsed greedily (maximal
munch). Unicode and ASCII op_chars may be freely mixed within one token.

**Reserved punctuation** (not operators): `→`, `⇒`, `←`, `:`, `,`, `;`,
`(`, `)`, `[`, `]`, `{`, `}`, `⟪`, `⟫`, `` ` ``.

**Reserved Unicode** (not available as user-defined operators):

| Code point | Character | Role |
|------------|-----------|------|
| U+2192 | `→` | function type / lambda separator |
| U+21D2 | `⇒` | constraint implication |
| U+2190 | `←` | binding |
| U+2200 | `∀` | universal quantifier |
| U+2203 | `∃` | sigma type introduction |
| U+27EA | `⟪` | effect row open |
| U+27EB | `⟫` | effect row close |
| U+002E | `.` | field access / update / unquote prefix |
| U+002E U+002E | `..` | range / stride operator (reserved two-char token) |
| U+22A5 | `⊥` | never / bottom type |

Every other non-whitespace, non-identifier Unicode code point is a valid
`op_char` and is available for user-defined operators.

**No Unicode normalization**: each code point is a distinct token. Visually
similar code points (e.g., ASCII `/` U+002F and division slash `∕` U+2215) are
distinct operators with independent hook sets. The programmer is responsible
for avoiding visually confusing choices.

**Reserved ASCII punctuation** (not operators, map to the above): `->`, `=>`, `<-`.

**Reserved comment prefixes**: `//` begins a line comment. `/'-` and `-'/`
delimit attributes. `/'` and `'/` delimit block comments. These take precedence
over operator parsing.

### 1.3 Literals

```ebnf
int     = decimal | hex | binary | octal

decimal = [0-9] ([0-9] | '_')*
hex     = '0x' ([0-9a-fA-F] | '_')+
binary  = '0b' ([01] | '_')+
octal   = '0o' ([0-7] | '_')+

float   = decimal '.' [0-9] ([0-9] | '_')* (('e' | 'E') ('+' | '-')? decimal)?

string  = '"' char* '"'
        | '"""' char* '"""'        (* raw multiline string *)

symbol  = '`' (lower | upper) (op_char | lower | upper)*
          (* e.g. `bid, `AAPL, `http://example.com, `some.dotted.path *)
```

Underscores may appear anywhere within a numeric literal for digit grouping:
`1_000_000`, `0xFF_FF`, `0b1010_0101`. Leading or trailing underscores are
not permitted.

Numeric type annotation uses the standard typed-pattern form rather than a
glued suffix: `(255 : U8)`, `(3.14 : F32)`. No dedicated suffix syntax.

**`Symbol` is a built-in primitive type** for runtime-interned identifier
constants. A symbol literal `` `AAPL `` has type `Symbol`. Equality is O(1)
(pointer comparison on interned integers). A symbol begins with a `lower` or
`upper` character and may continue with any mix of identifier and `op_char`
characters — this allows URLs, dotted paths, and hyphenated names as symbols
(`` `http://example.com ``, `` `some.dotted.path ``, `` `bid-ask ``). The
symbol is terminated by whitespace or any structural delimiter (`,` `;` `(`
`)` `[` `]` `{` `}`). The leading `` ` `` makes symbols unambiguous with
plain identifiers and with the quoted-program form `` `[ `` (backtick then
`[`), since `[` is a structural delimiter and cannot appear inside a symbol.

**`Char` is the primitive character type.** `String` is a type synonym
`alias String ← Char[]` — a string is an array of `Char`. A string literal
`"hello"` has type `Char[5]`; a single-character string `"a"` has type
`Char[1]`. No dedicated character literal syntax is needed — single-char
strings serve that role, and the type system tracks the length.

### 1.4 Comments

```ebnf
line_comment  = '//' (!newline ANY)*
block_comment = "/'" (block_comment | !("'/") ANY)* "'/"
```

Block comments are nestable. The `/' ... '/` delimiter was chosen to avoid
conflict with `/*` (multiply-then-scan), as `/'` is not a valid operator
sequence — `'` is not an `op_char`.

### 1.5 Attributes

```ebnf
attr       = "/'-" (upper value? ';'*)* "-'/"
attr_scope = attr '(' decl* ')'
```

Attributes annotate the expression immediately following them. Each attribute
is an `upper` name optionally followed by a single value argument. Multiple
attributes are semicolon-separated within one block. Example: `/'-Multi-'/`.

An attribute may also be followed by a paren-delimited block of declarations
(`attr_scope`), in which case it is inherited by every declaration inside the
block as if it appeared on each one individually. A declaration inside the
block may carry the same attribute explicitly — the inner declaration's
attribute takes precedence over the enclosing block's.

The **`Watch`** attribute is built-in and attaches a file-watcher ASCII trigger
to the definition that follows it. When that definition is in scope (imported),
the watcher rule is active:

```
/'-Watch "/:"-'/
op ÷ Float, Float → Float ← prim.float_div
```

The string argument is the ASCII trigger that the file watcher substitutes with
the Unicode operator name on save. See §2 and §9.7 for full details.

### 1.6 Unicode and Bracket Assignment

Arra uses Unicode to give each bracket pair a distinct structural purpose,
reducing ambiguities that would otherwise require contextual disambiguation.
Every Unicode form has an ASCII fallback; a file watcher (Uiua-style)
substitutes Unicode on save.

`( )` is the general scope bracket — it unifies grouping, tuples, lambdas,
trait bodies, module bodies, `rec` blocks, and do-notation under one form.
Content disambiguation is by top-level token: `→` signals a branch; `,`
signals a tuple; `;` or newline with no `→` signals a sequential block;
a single expression is grouping.

**Bracket pairs:**

| Unicode | ASCII fallback | Role |
|---------|---------------|------|
| `( )` | — | Grouping, tuples, scopes: lambdas, modules, traits, `rec` |
| `[ ]` | — | Arrays, pattern vectors |
| `{ }` | — | Records only (type and value) |
| `⟪ ⟫` U+27EA/B | ⚑ open | Effect rows |

**Arrows:**

| Unicode | ASCII | Role |
|---------|-------|------|
| `→` U+2192 | `->` | Function type; lambda body separator |
| `⇒` U+21D2 | `=>` | Constraint implication |
| `←` U+2190 | `<-` | Binding / "is defined as" |

**Quantifiers:**

| Unicode | ASCII keyword | Role |
|---------|--------------|------|
| `∀` U+2200 | `forall` | Universal quantification |
| `∃` U+2203 | `exists` | Sigma type introduction |

**Operator display substitutions** (file-watcher only; ASCII source remains
valid):

| ASCII | Unicode | |
|-------|---------|--|
| `<=` | `≤` U+2264 | inequality in constraints |
| `>=` | `≥` U+2265 | inequality in constraints |
| `/=` | `≠` U+2260 | not-equal |
| `++` | `⧺` U+29FA | row merge (optional) |
| `\o` | `∘` U+2218 | explicit composition (optional) |

### 1.7 Significant Newlines and Layout Rule

Arra uses a layout rule modelled on Haskell's: explicit scope brackets `( )`
can be elided when indentation makes structure unambiguous.

**Explicit form** — `( )` with `;` as statement separator:

```
square ← (x → x * x)
isZero ← (0 → True; _ → False)
(x ← readLine; y ← x ++ " world"; putLine y)
```

**Layout form** — `( )` elided; indentation drives parsing:

```
square ← x → x * x

isZero ←
  0 → True
  _ → False

x ← readLine
y ← x ++ " world"
putLine y
```

**Indentation unit**: Only ASCII space (U+0020) is a valid indentation
character. A tab character (U+0009) at any indentation-sensitive position is
a **lexer error**. Columns are 1-indexed and counted in Unicode scalar values
(code points). This makes column arithmetic portable and unambiguous.

**Block-introducing tokens**: The following tokens open a layout context when
NOT immediately followed by `(` or `;` on the same line:

| Token | Context opened |
|-------|---------------|
| `←` | right-hand side expression or block |
| `→` (branch/lambda separator only — see below) | branch body |
| `module` | module body |
| `interface` | interface body |
| `implementation` | implementation body |
| `trait` | trait body |
| `rec` | recursive binding or block |
| `effect` | effect operation list |
| `'handle` | handler block |
| `iso` | isomorphism body (two `from_impl` entries) |

**Lexer-level transformation**: The layout rule is implemented entirely in
the lexer. The lexer maintains a *context stack* of open layout contexts,
each recording a reference column. When the three-case rule fires, the
lexer injects virtual `;` tokens or pops contexts from the stack before
passing the token stream to the parser. The parser never inspects
indentation; it sees only real tokens and virtual `;` tokens. An explicit
`;` is always valid — it is identical to a virtual one. This is the same
split as Haskell's layout rule.

**Reference column**: To open a layout context, scan forward (across
whitespace and newlines) to the first non-whitespace, non-comment token.
That token's column is the *reference column* for the new context.

*Suppression — same-line `(` only*: if the first non-whitespace token
after the opener is `(` **and it is on the same line as the opener token**,
the layout context is suppressed entirely and `( )` delimiters provide
explicit structure. If `(` appears on the *next* line, the layout context
is opened normally with `(` as the reference column token; the inside of
the `( )` is then layout-free per the bracket-context rules.

```
f ← (x + y)        // ( same line → layout suppressed; ) closes scope

g ←                 // opener at end of line
  (x + y)           // ( on next line → layout context opened; C = col of (
                    // inside ( ) is layout-free; ) closes the parens
                    // next line at col < C closes the ← layout context
```

**Three-case rule**: The lexer maintains a stack of open layout contexts.
Within the innermost context (reference column C), when a newline is
encountered:

1. Scan to the first non-whitespace, non-comment character on the next
   non-blank line. Let N be its column.
2. **N = C** → inject a virtual `;` (new statement in the current context).
3. **N > C** → no action; the new line continues the current statement.
4. **N < C** → pop the current layout context; inject a virtual `;` into
   the next outer context if N equals its reference column, or continue
   popping if N is still less. Repeat until N ≥ the current context's C
   or the stack is empty.

Multiple consecutive blank lines collapse to one newline event.

**EOF**: when the end of file is reached, the lexer pops all remaining
layout contexts from the stack (each pop injects a virtual `;`) and then
emits the EOF token. An unterminated explicit `( )`, `[ ]`, or `{ }` is a
parse error, not a layout error.

**`→` in type position does not open a layout context**: `→` as the branch
or lambda separator (following a pattern or guard inside a value expression)
opens a layout context. `→` as the function-type arrow (in a type
annotation after `:`, or inside a `fun_type` inside `( )`) does not.
The parser determines which form applies from its current grammatical context
— no lookahead or backtracking is required.

**Single-line forms** fall out of the general rule without special casing.
In `square ← x → x * x`, the `←` opens a layout context at the column of
`x`, and `→` opens one at the column of `x * x`. Both reference columns are
on the same line. The next line starts at a lesser column (N < C), so both
contexts close before the next statement. The result is identical to the
explicit form `square ← (x → x * x)`.

**Multi-line expressions**: An expression may span multiple lines by
indenting continuation lines past the reference column (N > C). No special
operator-at-line-end rule is needed — the column alone determines whether a
new line is a continuation or a new statement.

```
// Continuation: indented past reference column of ←
result ←
  someVeryLong +
  expression

// Multi-line branch body: → opens layout at column of first statement
classify ←
  x when x > 0 →
    y ← transform x
    y * 2
  x when x < 0 → 0 - x
  _            → 0
```

**`iso` block layout**: the `iso` opener sets the reference column to the
first `from` keyword. Both `from_impl` entries must start at that column;
the virtual `;` between them is inserted by the three-case rule (N = C).

```
iso A B                        // iso opens layout context
  from A → B ← forward_fn     // reference column C = col of 'from'
  from B → A ← backward_fn    // N = C → virtual ';' separates entries
                               // next outer context closes here
```

**`'handle` block layout**: the `'handle` opener sets the reference column
to the first handler clause keyword (`ctl`, `op`, or `return`). Each clause
must start at that column; virtual `;` tokens separate them.

```
'handle computation
  ctl fail e →                 // reference column C = col of 'ctl'
    default                    //   body: N > C, continuation
  op yield v k →               // N = C → virtual ';', new clause
    k (transform v)            //   body: N > C, continuation
  return x →                   // N = C → virtual ';', new clause
    wrap x
```

**Newline rules by bracket context:**

- **Top level and layout scopes**: the three-case rule applies.
- **Inside explicit `( )`**: newlines are insignificant. Use `;` to separate
  statements; `,` to separate tuple elements.
- **Inside `[ ]` and `{ }`**: newlines are insignificant. Use `;` for array
  elements and record fields.

The layout rule is the default style. Explicit `( )` are used when structure
must be unambiguous inline or when a scope is passed as a value.

---

## 2. File Watcher

The file watcher performs automatic substitution of ASCII trigger sequences to
their Unicode equivalents on save. Substitutions are bijective — every Unicode
form has a unique ASCII trigger, and the mapping is lossless in both directions.
Source files may contain either form; the canonical on-disk form is Unicode.

**Tier 1 — structural punctuation** (always substituted; these are already
reserved and have no other interpretation):

| ASCII trigger | Unicode |
|--------------|---------|
| `->` | `→` |
| `=>` | `⇒` |
| `<-` | `←` |


**Tier 2 — keyword triggers** (substituted when appearing as a complete token
in type or hook context):

| ASCII trigger | Unicode |
|--------------|---------|
| `forall` | `∀` |
| `exists` | `∃` |

**Tier 3 — bracket pairs** (substituted when the opening trigger is followed
by a non-`op_char`; the closing trigger is substituted unconditionally):

| ASCII open | ASCII close | Unicode open | Unicode close | Role |
|-----------|------------|-------------|--------------|------|
| `<\` | `\>` | `⟪` | `⟫` | effect row |

The ASCII bracket trigger is visually distinctive and structurally suggestive
(`<>` for angle). Because `\` is an `op_char`, the sequence `<\` is a valid
operator prefix. The watcher avoids misfiring by requiring the trigger to be
followed by a non-`op_char` (whitespace, identifier character, or end-of-line)
before substituting. If the next character is an `op_char` the trigger is left
intact.

**Auto-close**: when the opening trigger fires, the watcher checks whether the
matching closing trigger (`\>`) already appears before end of line. If not, it
inserts the closing bracket at end of line. This gives the common editor
auto-pair experience without requiring cursor awareness — the programmer fills
in the content between the already-placed brackets.

**Tier 4 — operator display** (substituted within expressions and constraints;
the ASCII form remains the canonical operator token for parsing):

| ASCII | Unicode |
|-------|---------|
| `<=` | `≤` |
| `>=` | `≥` |
| `/=` | `≠` |
| `++` | `⧺` |
| `\o` | `∘` |

**Tier 5 — user-defined** (substituted when the definition bearing the `Watch`
attribute is in scope in the current file):

User-defined watcher rules are attached to hook definitions via the
`/'-Watch "trigger"-'/` attribute (§1.5). The trigger string is the ASCII
sequence the watcher replaces with the operator's Unicode name. Rules are
scoped to the import: when a definition is in scope (via `open`), its `Watch`
rule is active; when excluded (via `hiding` or selective import), it is not.

```
/'-Watch "/:"-'/
op ÷ Float, Float → Float ← prim.float_div    // /: rewrites to ÷ when Foo is open

/'-Watch "sum"-'/
iter ∑ a[n] → a ← (arr → arr 'fold (+) 0)   // sum rewrites to ∑
```

The trigger string must not conflict with Tier 1–4 triggers or other in-scope
Tier 5 rules. The language does not enforce this at compile time; conflicting
triggers in the same scope are resolved in import order (last `open` wins).

---

## 3. Programs

```ebnf
sep     = NEWLINE | ';'
program = (expr sep+)* expr?
```

A program is a sequence of expressions separated by one or more statement
separators. A separator is a newline or an explicit `;`. Trailing separators
are ignored. This is the top-level form and also the body of explicit `( )`
scopes and modules. Inside `[ ]` and `{ }`, newlines are whitespace and `sep`
does not apply (see §1.7).

`program` is used uniformly in both **definition scope** (the body of
`module`, `interface`, `implementation`, `trait`) and **value scope** (the RHS
of a binding, the body of a lambda). The grammar does not distinguish them;
**scope restrictions are enforced semantically**: definition-scope constructs
(`hook_def`, `type_def`, `distinct_def`, `type_syn`, `effect_def`,
`derived_def`) are rejected by the type checker if they appear inside a value
position. The error message names the enclosing scope.

> **Future**: local `module` declarations in value scope (first-class local
> namespacing, analogous to OCaml's `let module`) are not currently permitted
> but are structurally compatible with this grammar.

---

## 4. Expressions

An expression is an attributed or unattributed combination of a leading value
and an optional projection chain.

```ebnf
expr = attr? (type_sig | bind | value) projection?

type_sig = lower ':' type       (* standalone type declaration: name : Type *)

bind = pattern '←' expr                  (* value binding: name ← value *)
     | 'rec' pattern '←' expr            (* self-recursive binding *)
     | 'rec' '(' program ')'             (* mutually recursive block *)
```

**Strict definition ordering**: in every binding context, a name may only
be used after it has been defined. Top-to-bottom order is enforced uniformly
— there is no context where forward references are permitted without `rec`.
The two `rec` forms are the only exceptions, and they work identically in
all contexts:

- `rec name ← expr` — `name` may appear inside `expr` (self-recursion).
- `rec ( program )` — any binding in the block may reference any other
  binding in the same block, in any order. Only `bind` and `type_sig`
  expressions are valid inside a `rec ( )` block.

**Definition scope vs. value scope**: `( )` introduced by a definition
keyword (`module`, `interface`, `implementation`, `trait`) is a
*definition scope* — only `type_sig`, `bind`, `data_def`, `distinct_def`,
`type_syn`, `effect_def`, `derived_def`, `trait_def`, `hook_def`,
`iso_def`, `impl_def`, and nested `module_def` expressions are valid inside it.
`derived_def` is additionally restricted: it may not appear in an
`interface` or `trait` body (it is an implementation detail, not an API
contract). `( )` appearing in
expression position is a *value scope* — any `program` is valid, including
effectful expressions and arbitrary computation. Both scopes enforce the
same strict top-to-bottom ordering and the same `rec` escape rules; they
differ only in what content is permitted.

**Type signature rules** (Mercury-style):

- A `type_sig` must appear **before** its corresponding `bind` in source
  order — consistent with strict top-to-bottom ordering. No adjacency
  requirement: other statements may appear between them.
- A `type_sig` with no corresponding `bind` in the same scope is a compile
  error (incomplete definition), except in `interface` blocks where the
  binding lives in the paired `implementation` block.
- **Cross-block**: a `type_sig` in an `interface` block serves as the
  authoritative declaration for the matching `bind` in the `implementation`
  block. The implementation binding does not need to repeat the type sig;
  if it does, the compiler checks the two are consistent.
- A `bind` with no `type_sig` is always valid — the type is inferred.

**`:` is exclusively type annotation.** `x : Int` declares that `x` has type
`Int`. It is never used for value binding.

**`←` is exclusively value binding**, with the lowest precedence. `x ← 1 + 1`
binds `x` to `2`. The left-hand side is a full destructuring pattern. The
annotated binding form combines a typed pattern with a value:

```
(x : Int) ← 5          // x of type Int is bound to 5
```

**`←` as do-notation**: inside a `( )` scope, `←` sequences effectful
computations identically to pure ones. Effects propagate transparently through
the type system — no separate monadic bind syntax is needed.

```
// explicit scope:
(x ← readLine; y ← x ++ " world"; putLine y)

// layout form:
x ← readLine        // effectful: ⟪IO⟫ propagates to enclosing type
y ← x ++ " world"  // pure: no effect
putLine y
```

This is Arra's do-notation: the same `←` for pure and effectful bindings,
unified by the Koka-style effect system.

---

## 5. Values

Values are the atomic subjects of expressions.

```ebnf
value = paren_expr
      | section
      | quoted_program
      | unquoted_program
      | vector
      | symbol
      | string
      | int
      | float
      | lower
      | upper
      | tick
      | field_name
```

### 5.1 Parenthesized Expressions

`( )` is the unified scope bracket. Disambiguation is by the top-level tokens
inside — the parser resolves which form applies without backtracking:

```ebnf
paren_expr = '(' ')'                                         (* unit *)
           | '(' handler_clause (sep handler_clause)* ')'    (* handler block *)
           | '(' branch (sep branch)* ')'                    (* lambda / branch block *)
           | '(' program (',' program)+ ')'                  (* tuple (2+ elements) *)
           | '(' program ')'                                 (* grouping or sequential block *)

branch     = branch_pat guard? '→' tuple_expr
branch_pat = pattern ('|' pattern)*
guard      = 'when' expr

tuple_expr = program (',' program)*               (* bare tuple or single program *)
```

Disambiguation is by the first non-whitespace token inside `( )`:

| First token | Form |
|-------------|------|
| `ctl`, `op`, `return` | handler block |
| `→` at top level | branch / lambda |
| `,` at top level, no `→` | tuple |
| `)` | unit |
| else | grouping or sequential block |

`ctl`, `op`, and `return` are globally reserved (§1.1) and cannot appear as
pattern names, so the handler block form is detected without backtracking.

**Handler block form** — first token is `ctl`, `op`, or `return`: an effect
handler installed by `'handle`. Each clause handles one operation or the
return value. See §6 for the `handler_clause` grammar and semantics.

```
comp 'handle (ctl raise e → e Err; return x → x Ok)     // inline
comp 'handle (op yield v k → k (); return x → [x])      // resumable
```

Layout form (parens elided via §1.7):
```
comp 'handle
  ctl raise e   → e Err
  op  yield v k → k ()
  return x      → x Ok
```

**Branch form** — `→` present at top level: a lambda or multi-branch
pattern-matching block. A single branch is an anonymous function; multiple
branches dispatch on patterns. Multi-arg functions use tuple patterns.

```
(x → x + 1)                         // single-arg lambda
((x, y) → x + y)                    // two-arg via tuple pattern
(0 → "zero"; _ → "other")           // multi-branch, inline
((m, _, arr) → arr 'fold (+) 0)     // sigma elimination via tuple pattern
(x → x, x * x)                      // branch body is a bare tuple: returns (x, x²)
```

Layout form (parens elided):

```
// multi-branch, layout form:
0 → "zero"
_ → "other"
```

**Tuple form** — `,` present, no `→` at top level:

```
(a, b, c)         // 3-tuple
(a,)              // 1-tuple (trailing comma distinguishes from grouping)
(a + b, c * d)    // tuple of two computed expressions (program, not just value)
```

Bare tuples (without surrounding `( )`) are valid in positions where an explicit
outer delimiter makes `,` unambiguous — vector elements, call vector slots, and
branch bodies. See §5.2 and §5.5.

`()` is the unit value. `(e)` is grouping (no comma, no `→`). Tuples are
also the elimination form for sigma types — `(m, _, arr)` destructs an
existential in a branch (see §8.4).

**Sequential block / do-notation** — `;` or newlines, no `→` or `,` at top
level:

```
(x ← readLine; y ← x ++ " world"; putLine y)   // explicit scope
```

In layout form (§1.7), the `( )` are elided and indentation drives the block.

**Or-patterns**: bare `|` between alternatives before `→` — `→` is the right
delimiter so `|` is unambiguous. `|` after `→` (in the body) is bitwise-or.

```
(None | Nothing → 0; Some v → v)   // | before →: or-pattern
(x → x | mask)                     // | after →: bitwise or
```

**Guards** (`when expr`): optional boolean condition between pattern and `→`.
The guard may reference any name in scope. `→` terminates the guard.

```
(x when x > 0 → x; _ → 0)

// layout form:
x when x < lo → lo
x when x > hi → hi
x             → x
```

**Guard Z3 semantics**: when `expr` is a Z3-linear arithmetic predicate,
the guard condition refines the branch body's type context. Opaque predicates
are runtime-only; the branch is `⟪Fail⟫`-typed unless exhaustiveness is
otherwise proved.

Guards are the inline, local-scope form. Named `pat` hooks (§9.2) are
the module-level form for reusable, Z3-witnessed conditions.

### 5.2 Vectors (Arrays)

```ebnf
vector = '[' (tuple_expr (';' tuple_expr)*)? ']'
```

Semicolon-separated, following K/Q convention. `[1;2;3]` is a 3-element array.
`[]` is the empty array. Nested: `[[1;2];[3;4]]` is a 2×2 matrix.

Each element is a `tuple_expr`: `;` separates elements and `,` is unambiguous
within an element, so bare tuples are valid without extra parentheses:

```
[1, 2; 3, 4; 5, 6]      // vector of three 2-tuples; equivalent to [(1,2);(3,4);(5,6)]
[x, x*x; y, y*y]        // computed tuples
```

**Range and stride**: `..` is a built-in operator producing a `Range Int`
value (see SEMANTICS.md §2.1 for semantics).

```
0..n       // Range Int — half-open, exclusive end
1..10      // Range Int
1..2..10   // Range Int with step=2 (inclusive)
```

`1..2..10` parses LTR as `(1..2)..10`. Two hooks cover both forms:

```ebnf
op .. : Int, Int       → Range Int   (* simple range, step=1 *)
op .. : Range Int, Int → Range Int   (* strided: promotes .end → .step, sets new .end *)
```

### 5.3 Field Names

```ebnf
field_name = '.' lower
```

A field name literal `.x` has type `Field` — a runtime interned token that
identifies a named position in a record row type. Field names are first-class
values: they may be stored, passed, and abstracted over.

```
.x                          // field name token, type Field
r .x                        // project field x from r (same as r.x)
r f                         // computed access: f : Field variable
(r, f, v) assign             // computed update: f used directly as a Field token
```

`r.x` (§6) is syntactic sugar for `r .x`. The computed form `r f` is used
when `f : Field` is a variable. Both dispatch via the built-in `project`
hook for records.

**`from` coercion**: when a function type is expected and a `Field` value is
supplied, the stdlib `from Field → function` hook fires implicitly. No
explicit parenthesising is needed in typical tacit use:

```
arr 'map .x         // 'map demands (a → b); from fires; .x becomes field accessor
arr 'map (.x)       // explicit section: identical result
```

The stdlib entry (see SEMANTICS.md §3.5.2):

```
from ∀ (f : Field) T rest ⇒ f → ({f: T | rest} → T) ← prim.field_to_fn
```

**Sections**: `(.x)` is the explicitly-sectioned form. By normal section rules
it is equivalent to `(from .x)`, and both have type `∀ T rest ⇒ {x: T | rest} → T`.
Uses of `.x` in a `Field`-typed position — such as the field argument of
`(r, f, v) assign` — do not trigger `from`.

**`reflect`**: `.x` is already a runtime token. `reflect .x` is the identity
on `Field` values and is a no-op at runtime. It serves as an explicit annotation
for code that passes field tokens across module or FFI boundaries and wants to
document that the value is being used as a token rather than coerced to a function.

### 5.4 Sections and First-Class Symbolic Forms

```ebnf
section = '(' op ')'                       (* operator as value: (+) *)
        | '(' value op ')'                 (* left section:  (1+) = (x → 1 + x) *)
        | '(' op value ')'                 (* right section: (+1) = (x → x + 1) *)
        | '(' tick ')'                     (* tick as value: ('map) *)
        | '(' value tick ')'               (* tick left section *)
        | '(' tick proj_arg ')'            (* tick right section *)
        | '(' tick value proj_arg ')'      (* tick with extra value arg *)
```

```
('map)        // tick as value:       (arr f → arr 'map f)
(arr 'map)    // tick left section:   (f → arr 'map f)
('map f)      // tick right section:  (arr → arr 'map f)
('rank 1 f)   // tick + extra arg:    (arr → arr 'rank 1 f)
```

The `value` slot in `('rank 1 f)` is the extra value argument (see §6
`sym_proj`); `proj_arg` is the function argument. The two are positionally
fixed by the `sym_proj` grammar, so the section form mirrors it exactly.

Tick sections are first-class functions and compose via `∘` (atop), enabling
point-free pipeline construction without introducing intermediate names:

```
('map f) ∘ ('map g)        // pipeline: first 'map f, then 'map g
('map parse) ∘ ('sort)     // first parse each element, then sort the result

pipeline ← ('map parse) ∘ ('filter valid) ∘ ('sort)
data 'each pipeline        // apply the composed pipeline
```

**`'handle` section forms**: of the four tick section forms, only the
right-section `('handle handler_block)` is valid. It produces a function
from computation to handled computation — the primary way to define
reusable named handlers (see SEMANTICS.md §5.1.2):

```
withFail : ⟪Fail e | fx⟫ a → ⟪fx⟫ (Result e a)
withFail ← ('handle (ctl raise e → e Err; return x → x Ok))
```

The bare-tick `('handle)` and left-section `(computation 'handle)` forms
require handler blocks to be first-class runtime values; since handler
blocks are currently syntactic forms rather than runtime values, these two
forms are type errors. **Forward-compatible design**: `handler_block` is
already a valid `proj_arg`, so the parser accepts all four section forms —
only the type checker enforces the current restriction. If a first-class
`Handler` type is adopted (SEMANTICS.md §5.1.2 Option C), no grammar
change is needed.

### 5.5 Quoted Programs

```ebnf
quoted_program   = '`[' program ']'
unquoted_program = '.(' program ')'
```

`` `[expr] `` produces a `Program T` value — a first-class, typed representation
of an unevaluated program form (SEMANTICS.md §7). `.(expr)` splices
`expr : Program T` back into a `T`-typed position; a splice at a non-quoted
position is evaluated at compile time.

Since types and values share the same grammar, type expressions are quoted and
inspected by the same mechanism as value expressions — no separate quotation
family is needed.

---

## 6. Projections

A projection follows a value and extends the expression. Projections chain
left-to-right, implementing LTR evaluation order. This is the **universal
and only** precedence rule in Arra: all operators share equal precedence and
associate strictly left-to-right. No precedence declarations are permitted —
neither for built-in operators nor for user-defined ones.

```ebnf
projection = sym_proj
           | app_proj
           | field_access

sym_proj = (op | tick) (value? proj_arg)? projection?
app_proj = value projection?

field_access = '.' lower projection?
             | '.' int   projection?       (* tuple index: t.1, t.2 — 1-indexed *)
             | '.(' update_list ')'

update_list = lower '←' expr (';' lower '←' expr)*

proj_arg = lambda
         | op
         | lower
         | upper
         | '(' projection ')'
         | '(' program ')'
         | handler_block

lambda = pattern_atom guard? '→' program   (* unparenthesised lambda in proj_arg position *)

handler_block  = '(' handler_clause (';' handler_clause)* ')'
handler_clause = 'ctl' lower pattern* '→' program
               | 'op'  lower pattern* lower '→' program
               | 'return' pattern '→' program
```

**`sym_proj`** covers all symbolic applications — both operators and modifiers,
since they are one syntactic category. When the right-argument group
`(value? proj_arg)` is **present**, the operator dispatches as `bop`, `iter`,
or `gen` depending on `proj_arg`'s kind. When it is **absent** (nothing follows
the operator that could start a `proj_arg`), the operator dispatches as `uop`
on the left value's type — postfix unary: `x-`, `x!`. The optional `value`
before `proj_arg` handles tick hooks that take an extra value argument
(e.g., `arr 'rank 1 f`); it is only present on tick forms.

**Disambiguating `value?` from `proj_arg`**: non-`lower` tokens in `value?`
position (literals, `upper` names, parenthesised forms) are unambiguous by
grammar. The ambiguous cases involving bare `lower` names following a tick are
resolved by two rules applied in order:

> **Lambda rule**: after a tick, if the next tokens form a valid `pattern_atom`
> immediately followed by `→` or `when`, parse as an unparenthesised `lambda`.
> `lambda` is first in the `proj_arg` ordered choice, so this takes priority.
> Valid unparenthesised lambda heads: `_`, a `lower`, a nullary or unary `upper`,
> a parenthesised pattern, a `[`-pattern, a `{`-pattern, or a literal.

> **Two-consecutive-lower rule**: after a tick, if the next token is a `lower`
> and the token immediately following it is also a `lower` (and not `→` or
> `when`), parse the first as `value?` and the second as `proj_arg`.
> Otherwise parse the single `lower` directly as `proj_arg`.

Consequence: `arr 'map f g` → `value?=f, proj_arg=g`; `arr 'map x → x + 1`
→ `proj_arg=lambda`. To apply the result of a modifier to a subsequent
argument, parenthesise the modifier expression: `(arr 'map f) g`.

**Unparenthesised lambda body extent**: the `program` body of a `lambda` in
`proj_arg` position extends to the enclosing layout context — the end of the
current indented block or an explicit `)`. In layout form, each tick on a new
line naturally closes the preceding lambda body; on a single line, use
parentheses to stop the body early. As-pattern and shape-annotation suffixes
(`@ name`, `[n]`) on the lambda head require the whole lambda to be
parenthesised: `arr 'map ((x, y) → body)`.

**`app_proj`** is Forth-style juxtaposition: the leading value is the
**argument**; the right-hand value is the **function** applied to it.
`f g` = `g(f)`. Chains are left-to-right pipelines: `f g h` = `h(g(f))`.
There is no implicit composition for juxtaposed functions; use explicit `∘`.

**Multi-arg calls** use tuple application: `(a, b, c) f`. The tuple is a
first-class value; the function comes last. For partial application, use an
explicit lambda: `b → (a, b, c) f`.

Parsed examples:

```
f g               // f | g       = g(f)            (arg then fn)
f g h             // f | g | h   = h(g(f))         (pipeline)
arr sort          // arr | sort   = sort(arr)       (single-arg call)
arr sort head     // arr | sort | head              (chained pipeline)
(a, b, c) f        // multi-arg call: tuple applied to function
a + b             // a | + b                       (sym_proj, no value slot)
a + b * c         // a | + b | * c                 (→ (a+b)*c, LTR)
arr 'map f        // arr | 'map f                  (single lower → proj_arg)
arr 'rank 1 f     // arr | 'rank 1 f               (literal value?, unambiguous)
arr 'rank n f     // arr | 'rank n f               (two lowers → value?=n, proj_arg=f)
arr 'map f + g    // arr | 'map f | + g            (lower then op → new sym_proj)
(arr 'map f) g    // (arr | 'map f) | g            (parens break grouping)
arr 'map x → x + 1             // proj_arg=lambda     (lower → lambda)
arr 'map _ → 0                 // proj_arg=lambda     (wildcard → lambda)
arr 'map None → default        // proj_arg=lambda     (nullary constructor → lambda)
arr 'map Some x → x            // proj_arg=lambda     (constructor + lower → lambda)
arr 'filter x when x > 0 → x  // proj_arg=lambda with guard
arr 'zip ((x, y) → x + y)      // tuple pattern: whole lambda parenthesised
arr 'map Some x[n] → x         // constructor + shaped sub-pattern: no parens needed
arr 'map Some x @ inner → x    // as-pattern on sub-pattern: no parens needed
```

**Note**: `arr 'fold (+) 0` is already handled — `0` is an `app_proj` applied
to the result of the `sym_proj` `arr 'fold (+)`, via the `projection?` chain
in `sym_proj`. No grammar extension is needed. The semantic question (whether
`arr 'fold f` is a complete fold or a partial application waiting for an
initial value) is deferred to SEMANTICS.md §9.

**`field_access`** has two forms:

- `r.x` — structural access: sugar for `r .x`. `.` binds tighter than
  juxtaposition; chains are left-associative: `r.a.b` = `(r .a) .b`.
- `r.(x ← v)` — structural update: return a copy of `r` with field `x`
  set to `v`. Multiple fields: `r.(x ← v; y ← w)`. Names inside `.()`
  are field name literals (not variable references). Desugars to
  `(r, .x, v) assign`.

**Disambiguation**: `.` followed by `lower` has three possible meanings,
resolved by parser position — no backtracking needed:

| Position | Form | Meaning |
|---|---|---|
| value position | `.x` | field name literal (kind `Field`) |
| value position | `.(prog)` | unquoted macro splice |
| projection position | `r.x` | field access (sugar for `r .x`) |
| projection position | `r.(x ← v)` | field update |

```
r.x                  // r .x             (field access)
r.a.b.c              // ((r .a) .b) .c   (chained)
r.(x ← 5)            // update field x to 5
r.(x ← v; y ← w)     // update two fields
r f                  // computed access, f : Field variable
(r, f, v) assign      // computed update, f : Field variable
.x                   // field name literal as standalone value
(.x)                 // field name as a first-class function value
```

**`handler_block`** is the argument form exclusive to `'handle`. It is a
`paren_expr` whose first token is one of the globally reserved keywords
`ctl`, `op`, or `return` — this is what distinguishes it from a branch
block or sequential scope inside `( )` (see §5.1 disambiguation table).
A handler block installs an algebraic effect handler over the preceding
computation (SEMANTICS.md §5.1.2). Three clause kinds:

- **`ctl` clause**: `ctl opName pattern* → program` — handles a
  non-resumable control operation. `opName` matches the operation declared
  `ctl` in the effect definition; `pattern*` binds the operation's
  arguments. There is no continuation — `ctl` operations abort the
  computation's dynamic extent.

- **`op` clause**: `op opName pattern* k → program` — handles a resumable
  operation. The final `lower` (`k`) is the explicit continuation: a
  function from the operation's declared return type to the handler's result
  type. `k` may be called zero times (prune), once (deterministic resume),
  or many times (enumerate all branches for `⟪Nondet⟫`).

- **`return` clause**: `return pattern → program` — transforms the value
  returned by the handled computation when it exits normally. Optional;
  defaults to identity `return x → x`.

`ctl`, `op`, and `return` are globally reserved identifiers (§1.1).

**Both explicit and layout forms** are valid (since `'handle` is a
block-introducing token in §1.7). Inside explicit `( )`, clauses are
`;`-separated; in layout form, indentation drives clause separation:

```
// Explicit ( ) form
comp 'handle (ctl raise e → e Err; return x → x Ok)

// Layout form
comp 'handle
  ctl raise e   → e Err
  op  yield v k → k () ++ k ()
  return x      → x Ok
```

---

## 7. Patterns

Patterns appear in bindings, branch patterns, lambda params, and hook
signatures.

```ebnf
pattern      = pattern_atom pattern_suffix*

pattern_atom = '_'                                    (* wildcard *)
             | lower                                  (* variable binding *)
             | upper pattern?                         (* constructor / pat hook *)
             | '(' pattern ('|' pattern)+ ')'         (* or-pattern: (A | B | C) *)
             | '(' pattern (',' pattern)* ')'         (* tuple / sigma elimination *)
             | '(' pattern ':' type ')'               (* typed pattern: (x : T) *)
             | '[' list_pat? ']'                      (* array / sequence pattern *)
             | '{' record_pat? '}'                    (* record / dict pattern *)
             | '(' expr '→' pattern ')'               (* view pattern: apply expr, match result *)
             | int | float | string                   (* literal match *)

pattern_suffix = '[' ('_' | (expr (';' expr)*))? ']'  (* shape annotation: x[n], x[n;m] *)
               | '@' lower                            (* as-pattern: bind whole matched value *)

list_pat = list_elems (';' '..' lower?)?              (* prefix elements + optional rest *)
         | '..' lower? (';' list_elems)?              (* rest first + optional suffix elements *)

list_elems = pattern (';' pattern)*

record_pat  = record_field (',' record_field)* (',' '..')?   (* named fields; '..' = open *)
record_field = lower (':' pattern)?                           (* {x} or {x: p} *)
```

The typed pattern `(x : T)` is the primary mechanism for expressing types in
hook dispatch — the right-hand side is a `type` expression (§8), not a
pattern. Function types in typed patterns require explicit parentheses:
`(f : (Int → Float))`. The shape annotation suffix `[n]`, `[n;m]` constrains
array dimensions in hook heads.

**As-patterns** (`pattern @ name`): binds the whole matched value to `name`
while also matching the structure of `pattern`. Pipeline-ordered: the
destructuring comes first, the name binding comes after.

```
[head; ..rest] @ arr   // bind the whole list to arr, also bind head and rest
{x, y} @ pt            // bind the whole record to pt, also bind x and y
```

**List patterns** — `..` marks the rest element; it may appear at most once:

```
[a; b]           // exactly two elements
[head; ..tail]   // head + rest (any length ≥ 1)
[..init; last]   // rest + last element
[a; ..mid; z]    // first, rest, last
[..]             // any list (unnamed rest — equivalent to _)
```

**Record patterns** — `..` makes the match open (extra fields allowed):

```
{x}              // closed: matches records with exactly field x
{x, y}           // closed: exactly fields x and y
{x: 0, ..}       // open: field x is zero, any other fields
{x: p, ..rest}   // open: field x matches p; remaining fields bound as record to rest
```

**View patterns** (`(expr → p)`): applies `expr` to the scrutinee and matches
the result against `p`. The expression is any value-level term producing a
function; purity and type correctness are enforced by the type checker, not
the parser. The `(expr → p)` form is unambiguous: in **pattern position** it
is a view pattern; in **expression position** `(expr → ...)` is a lambda or
branch (§5.3). The parser knows which context it is in from the enclosing
non-terminal.

```
(length → 0)            // scrutinee has length zero
(validate → Ok x)       // apply validate, destructure result
(abs → (n : Int))       // apply abs, match typed result
(head → Some first)     // head returns Option, match Some
```

**View patterns and `pat` hooks**: `pat` hooks (§9.2) are the named,
hookable form of the same mechanism. `Validated x` in pattern position is
equivalent to `(validatedImpl → x)` where `validatedImpl` is the `pat`
hook's resolved implementation. View patterns are anonymous and inline;
`pat` hooks are reusable, appear in `trait`/`interface` signatures, and
participate in hook dispatch.

**Constructors vs. `pat` hooks**: `upper pattern?` in pattern position
covers both data constructors and `pat` hooks (§9.2). The parser makes
no distinction; the type checker resolves which `upper` name refers to a
constructor and which to a `pat` hook. The sub-pattern is a full
`pattern` (including suffixes), so `Some x[n]` and `Some x @ inner` parse
without parens. Parens are only needed for tuple sub-patterns (`Some (x, y)`)
or to move an `@` binding to the constructor level: `(Some x) @ whole`.

**Or-patterns** (`p1 | p2`) come in two forms:

- **Branch form** (§5.3): `branch_pat = pattern ('|' pattern)*` — bare `|`
  is valid between alternatives; `→` is the right delimiter.
- **All other contexts**: or-patterns require parentheses — `(A | B)` is a
  `pattern_atom`. This prevents conflict with `|` in `enum_def` constructor
  lists and record row tails. `(A)` remains plain grouping; `(A | B)` with
  at least two alternatives is an or-pattern.

Param vectors (`param_vec`) take only simple `lower` names — or-patterns are
not valid there. Use the branch form instead.

```
(None | Nothing) ← lookup k m     // or-pattern in binding position
(A | B | C) ← classify x          // three alternatives
```

---

## 8. Types

Types share the same syntactic universe as values. The grammar is unified; the
type checker (not the parser) enforces which expressions are valid types. Types
may appear in quoted programs and are manipulated by macros using the same
machinery as value expressions.

The grammar below is the full type grammar. Types appear in typed patterns
`(x : T)`, after `→` and `⇒` at the top level of hook heads, and in
`type`/`trait` definition bodies.

```ebnf
type = constrained_type

constrained_type = '∀' tvar+ '⇒' fun_type
                 | fun_type

fun_type = app_type ('→' effect? fun_type)*

effect       = '⟪' (effect_entry ('|' effect_entry)*)? '⟫'
effect_entry = upper app_type*    (* named effect with ≥0 type args: ⟪IO⟫, ⟪State DB⟫ *)
             | lower              (* effect row variable: e, fx *)

app_type    = atomic_type+                              (* type application by juxtaposition *)
atomic_type = base_type ('[' dim (';' dim)* ']')?       (* array subscript binds tightly to base *)

base_type = upper                                   (* named type: Int, Bool *)
          | lower                                   (* type variable: a, b *)
          | '⊥'                                    (* never / bottom type; ASCII alias: Never *)
          | '(' type ')'                            (* grouping *)
          | '(' type (',' type)+ ')'                (* product type: (A, B) *)
          | sigma_type
          | record_type

sigma_type  = '∃' '(' lower ':' type ',' expr ')' base_type
              (* ∃(m : Nat, m ≤ n) a[m] *)
              (* binder, constraint, result type *)
              (* constraint is validated by a decidability analysis pass — see below *)

record_type = '{' (field (',' field)*)? '}'              (* closed record *)
            | '{' field (',' field)* '|' lower '}'       (* open record: row tail variable *)
            | record_type '++' record_type               (* disjoint row concatenation *)
            | record_type '-' field_name                 (* row subtraction *)

field = lower ':' type

dim = lower                (* dimension variable: n, m *)
    | int                  (* literal: 3 *)
    | dim '+' dim          (* sum: n+m *)
    | dim '*' dim          (* product: n*m (best-effort via Z3) *)
    | dim '-' dim          (* difference: n-1 *)

tvar = lower                           (* variable, kind inferred from usage *)
     | '(' lower ':' kind ')'          (* explicit annotation (documentation only) *)

kind = 'Type' | 'Nat' | 'Effect' | 'Row' | 'Field'
     | upper                              (* trait constraint: (a : Functor) *)
```

All `tvar` primitive kinds (`Type`, `Nat`, `Effect`, `Row`, `Field`) are
inferred by the type checker from structural position — no annotation is
required. The `(n : Nat)` form is accepted as a documentation aid and checked
for consistency if present.

**Trait names in `kind` position** are semantically significant: `(a : Functor)`
in a `type_constraint` declares a **supertrait requirement** — any type
implementing the enclosing trait must also implement `Functor`. This applies
only when `type_constraint` appears on a `trait_def`; on a `hook_def` or
`impl_def`, trait names in kind position express a **constraint on the type
variable** at that use site (the existing behaviour). See SEMANTICS.md §3.5 for
the full inference rules and the position-to-kind table.

### 8.1 Key Type Forms with Examples

**Function types:**

```
Int → Bool                    // pure function
String → ⟪IO⟫ String          // function with IO effect
a → ⟪IO | State⟫ b            // two effects
a → b                         // pure (no effect annotation = ⟪⟫)
```

**Array types:**

```
Int[n]                        // rank-1 array of Int with size n
a[n;m]                        // rank-2 array, sizes n and m
a[]                           // dynamic size (escape hatch)
```

**Record types:**

```
{x: Float, y: Float}          // closed record
{name: String | rest}         // open record: has name plus row tail variable rest
{x: Int} ++ {y: Float}        // disjoint row concatenation: {x: Int, y: Float}
{x: Int, y: Float} - .x       // row subtraction: {y: Float}
```

**Field name abstraction:**

```
∀ (f : Field) T rest ⇒ {f: T | rest} → T
// generic field accessor: takes any record containing field f, returns T

∀ (f : Field) T rest ⇒ {f: T | rest} → f → T → {f: T | rest}
// generic assign: container, field name, new value → updated container
```

**Sigma types:**

```
∃(m : Nat, m ≤ n) a[m]        // m exists, m ≤ n holds, result is a[m]
```

The `∃` prefix is the unambiguous signal for a sigma type. Without `∃`, a
parenthesised sequence with `:` is a typed pattern or annotated tuple, not a
sigma binder.

The constraint (second component) accepts any `expr`. A **decidability
analysis pass** runs before type-checking: it walks the constraint and
recursively inspects every function call in the chain, checking that each
callee is pure, total, and ultimately reduces to Z3's decidable fragment
(linear arithmetic, equality, boolean connectives). If any call in the chain
reaches an effectful or opaque function, the compiler rejects the constraint
with a targeted error naming the offending call. User-defined functions that
reduce to arithmetic (e.g. `paddedSize`, `nextPow2`) are therefore valid in
sigma constraints. Timeout and `unknown` results from Z3 itself are handled
per the policy in SEMANTICS.md §2.8.

Sigma elimination uses the tuple pattern in a branch:

```
(arr, pred) filter
  (m, _, filtered) → filtered 'fold (+) 0
// m : Nat in scope; filtered : a[m]; Z3 knows m ≤ n
```

**Constrained polymorphism:**

```
∀ a b ⇒ a[n] → (a → b) → b[n]            // universally quantified type
∀ a b e ⇒ a[n] → (a → ⟪e⟫ b) → ⟪e⟫ b[n]  // with effect variable
```

---

## 9. Definitions

All of the following are expressions and may appear anywhere a program can
appear. At the top level they constitute the module body.

### 9.1 Value Bindings

Function definitions are bindings where the right-hand side is a lambda.
Lambdas use the branch form inside `( )`, with the layout rule (§1.7) usually
eliding the `( )`. In projection position (§6), a single-argument lambda whose head is a
`pattern_atom` may appear unparenthesised: `arr 'map x → x + 1`,
`arr 'map None → default`, `arr 'map Some x → x`. Patterns with
`pattern_suffix` (`@ name`, `[n]`) require the whole lambda in parens.

```
square ← x → x * x
dot    ← (xs, ys) → xs * ys 'fold (+) 0
```

Type signatures are separate `type_sig` declarations using `:` (§4). At the
top level, newlines separate declarations — no trailing `;` needed:

```
square : Int → Int
square ← x → x * x

dot : Int[] → Int[] → Int
dot ← (xs, ys) → xs * ys 'fold (+) 0
```

Self-recursive functions use `rec`:

```
rec fib ←
  0 → 1
  1 → 1
  n → ((n-1) fib) + ((n-2) fib)
```

Mutually recursive functions use a `rec ( )` block. All bindings in the block
are in scope for each other regardless of their order within the block:

```
rec
  even : Int → Bool
  even ← n → n (0 → True; _ → (n-1) odd)

  odd : Int → Bool
  odd ← n → n (0 → False; _ → (n-1) even)
```

### 9.2 Hook Definitions

Each keyword names a **syntactic position** where type-directed dispatch fires.
The eleven keywords cover three optic pairs plus a Getter and a bidirectional
Iso, and symbolic arity forms — see SEMANTICS.md §3.1 for the full optics
table and dispatch direction rules.

```ebnf
hook_def  = hook_kw type_constraint? hook_head '←' program
              | iso_def

hook_kw   = 'bop' | 'uop' | 'iter' | 'gen'
              | 'app' | 'project' | 'assign' | 'view'
              | 'pat' | 'from'

hook_head = (op | tick) effect_param? pattern (',' pattern)*  (* symbolic/tick forms *)
              | upper pattern (',' pattern)*                      (* pat hooks *)
              | effect_param? pattern (',' pattern)*              (* project / from / assign / view *)

effect_param  = '[' lower ']'         (* effect variable: [e] *)

type_constraint = '∀' tvar+ '⇒'

(* iso_def declares a bidirectional coercion pair and opens a layout context: *)
iso_def = 'iso' type_constraint? pattern ',' pattern
          '(' from_impl ';' from_impl ')'

from_impl = 'from' pattern '←' program   (* one direction of the isomorphism *)
```

The `←` separates the full type signature from the implementation; `←` reads
as "is implemented by". Arity constraints per keyword:

- `uop`: one pattern (operand only — no right argument; call site is postfix `val sym`)
- `bop`, `iter`, `gen`, `project`, `view`: two patterns (left, right/field)
- `assign`: three patterns (container, field/index, new value)
- `from`: one pattern (source type; result type is output-dispatch target)
- `iso`: block form — two type patterns (A, B) plus a body containing exactly two `from_impl` entries, one for each direction
- `app`, `pat`: two patterns using the structural form

The grammar accepts any pattern count; arity is enforced by a **dedicated
post-parse pass** that runs before type-checking. This pass walks each
`hook_def` node, checks the keyword against the expected count, and emits
a targeted error (e.g. "`assign` requires exactly 3 patterns, got 2") before
unification is attempted.

**User-defined Unicode operators** use the same syntax with a Unicode `op_char`
as the hook name (SEMANTICS.md §3.6). `Watch` attributes (§1.5) attach
file-watcher ASCII triggers. Sections `(÷)`, `(1÷)`, `(÷2)` work for
user-defined operators by the same rules as built-in operators (§5.4).

**`pat` hooks** define user-extensible patterns — functions that appear in
pattern position. A `pat` hook takes a value and returns `Option U`: success
binds `U` in the branch; failure means the branch does not match. The name is
an `upper` identifier, making `pat` patterns syntactically indistinguishable
from constructor patterns — the type checker resolves which. When the return
type is a sigma type `Option ∃(x, constraint) U`, the Z3 witness is in scope
inside the matching branch (see SEMANTICS.md §3.7).

**`from`, `gen`, and `iso`** are output-dispatch: the hook is selected
by the result type, not the argument type. A type annotation or inference
context must determine the result type before resolution (SEMANTICS.md §3.1,
§3.5.2). `iso` additionally registers the inverse direction and the round-trip
laws; the two `from` hooks it generates participate in normal
output-dispatch resolution.

**`view`** is input-dispatch on the container type, identical to `project`, but
the declared projection is intrinsically read-only — no `assign` may be defined
for a `view` name (SEMANTICS.md §3.5.4).

```
// Binary operator (bop)
bop + Int, Int → Int ← prim.add_int

// Unary operator (uop): negate — call site is postfix: x-
uop - Int → Int ← prim.neg_int
// call site: x-  (dispatches uop on type of x)

// Effect-polymorphic fold/map (iter)
iter [e] 'map a[n], (a → ⟪e⟫ b) → ⟪e⟫ b[n] ← prim.vec_map

// Unfold/generate into array (gen) — dispatches on result type
gen [e] 'generate (A → ⟪e⟫ Option (A, T)), A → ⟪e⟫ T[] ← prim.vec_unfold

// Application hook (callable type)
app ∀ n ⇒ a[n], Nat → a ← prim.vec_index

// Field/index read (project) — user-defined types
project ∀ (f : Field) T rest ⇒ {f: T | rest}, f → T ← prim.rec_get
project ∀ n T ⇒ T[n], Nat → T ← prim.vec_get

// Type conversion / literal polymorphism (from) — dispatches on result type
from Int → Float ← prim.int_to_float
from Int → ∀ (a : Num) ⇒ a ← ...    // numeric literal polymorphism

// pat hook: runtime check only
pat Positive Int → Option Int ← n → (n > 0) (True → n Some; False → None)

// pat hook: Z3-witnessed refinement
pat Positive Int → Option ∃(k : Nat, k > 0) Int ← n → (n > 0) (True → n Some; False → None)

// assign hook: record field update (sugar: r.(x ← v))
assign ∀ (f : Field) T rest ⇒ {f: T | rest}, f, T → {f: T | rest} ← prim.rec_assign

// assign hook: array element update
assign ∀ n T ⇒ T[n], Nat, T → T[n] ← prim.vec_assign

// view hook: computed read-only field (no assign may be defined for .area)
view ∀ ⇒ Circle, .area → Float ← c → c.r * c.r * pi

// iso hook: bidirectional coercion with round-trip law
iso ∀ ⇒ Celsius, Float
  from Float → Celsius ← c → prim.celsius_wrap c
  from Celsius → Float ← c → prim.celsius_unwrap c
```

**Incrementalization annotations for `iter` hooks** inform `derived` view
maintenance (SEMANTICS.md §6.2). They attach to an `iter` definition:

- `/'-CMonoid -'/`: the accumulator type implements `CMonoid`; the compiler
  derives the delta function automatically from `+`, `-`, and `zero`.
- `/'-Delta f -'/`: an explicit delta function `f` handles updates. In debug
  builds the compiler inserts a consistency assertion (incremental result vs.
  full recompute) after each application.
- `/'-SkipDeltaCheck -'/`: suppresses the debug assertion on a delta function
  when the full recompute is impractical or the incremental result is the
  definition of correctness.

If no annotation is present on an `iter` hook used in a `derived`
expression, the compiler falls back to full recompute and emits a note.

```
iter /'-CMonoid -'/
'max : (a : Ord)[n], () → a ← heap → heap heapTop

iter /'-Delta bstDelta -'/
'sort : (a : Ord)[n], () → (a : Ord)[] ← prim.sort
```

### 9.3 Type Definitions

```ebnf
type_def    = 'type' upper lower* ('←' type_body)?

type_body   = record_def | enum_def

record_def  = '{' (field (',' field)*)? '}'

enum_def         = '|'? constructor_decl ('|' constructor_decl)*
constructor_decl = upper atomic_type?
```

`←` separates the type name from its definition body, consistent with value
bindings. The `←` and body are optional — bare `type Foo` declares a phantom
type (or an abstract type when in an `interface` block). The leading `|` in
`enum_def` is optional. Fields in `record_def` use `:` for type annotation,
consistent with `:` meaning only type annotation.

```
type Point ← { x: Int, y: Int }
type Option a ← None | Some a
type Marker                      // phantom type: no ← body
```

**Zero-cost wrappers** use `distinct`. A `distinct` definition creates a
distinct type with the same runtime representation as the underlying type —
no overhead, no boxing. Required when defining hooks for a type you do
not own (the orphan mechanism; see SEMANTICS.md §2.4 and §3.4).

```ebnf
distinct_def = 'distinct' upper lower* '←' type
```

```
distinct Celsius ← Float
distinct NonEmpty a ← List a
```

**Type synonyms** use `alias`. An `alias` definition is a transparent alias —
the two sides are interchangeable everywhere, including in hook dispatch.

```ebnf
alias_def = 'alias' upper lower* '←' type
```

```
alias String ← Char[]           // string: array of Char (standard library)
alias Point2 ← {x: Float, y: Float}
alias Predicate a ← a → Bool
```

Both `distinct` and `alias` follow the same type signature rules as `type` (§4):
`←` separates the name from the definition, type parameters follow the name
as bare lowercase names, and the definition must precede any use in source order.

**Transparent nominal types** — record `type` definitions automatically
generate a zero-cost `from` coercion to their underlying row type (see
SEMANTICS.md §2.3). This makes `type T ← {...}` structurally compatible with
row-polymorphic functions without an explicit bridge. The three forms are:

```
// Erasing: type synonym, purely structural, no nominal identity
alias Trade ← {sym: Symbol, price: Float, ts: Timestamp}

// Transparent nominal (default): nominal identity + implicit structural coercion
// compiler auto-generates: from Trade → {sym: Symbol, price: Float, ts: Timestamp}
type Trade ← {sym: Symbol, price: Float, ts: Timestamp}

// Opaque nominal: identity only, no structural coercion generated
/'-Opaque-'/ type Password ← {hash: Bytes, salt: Bytes}
```

The auto-generated `from` is zero-cost when field layout is canonical
(lexicographic order, the default). If `/'-FieldOrder-'/` reorders fields, the
coercion requires a field shuffle and the compiler emits a note.

### 9.4 Effect Definitions

```ebnf
effect_def = 'effect' upper lower* '(' effect_op+ ')'
effect_op  = ('ctl' | 'op') lower ':' type
```

`effect_def` declares a named algebraic effect — a set of operations that
computations may perform and handlers may intercept. The effect name is an
`upper` identifier, a ground constant of kind `Effect`. Lowercase names
following the effect name introduce type parameters; `effect Fail e`
gives `Fail : Type → Effect`.

Each `effect_op` is either:

- **`ctl`**: non-resumable. The return type in the signature must be `⊥` —
  a `ctl` operation aborts the computation's dynamic extent and never
  returns to the call site.
- **`op`**: resumable. The return type is what the call site receives when
  the handler calls the continuation `k`.

`effect` is a block-introducing token (§1.7): both explicit `( )` and
layout forms are valid. Operations are `;`-separated in the explicit form;
indentation governs in layout form.

```
// Parametric non-resumable: Fail : Type → Effect
effect Fail e (ctl raise : e → ⊥)

// Non-parametric resumable
effect Nondet (op yield : a → ())

// Multiple operations, layout form
effect IO
  op readLine  : () → String
  op writeLine : String → ()
  op readFile  : String → Bytes
```

Operations declared in an `effect_def` become ordinary functions in scope.
Calling an operation propagates its effect into the enclosing type
automatically — no annotation is required at the call site. See
SEMANTICS.md §5.1.1 for the full semantics.

`effect_def` is valid at module top-level, inside `interface` blocks (to
export an effect type), and inside `implementation` blocks (for private
effects). It is not valid inside `trait` bodies.

### 9.5 Trait Definitions and Implementations

```ebnf
trait_def = type_constraint? 'trait' upper lower* '(' sig+ ')'

sig = sym_hook_kw (op | tick) effect_param? ':' pattern (',' pattern)* ('←' program)?
    | 'pat' upper ':' pattern (',' pattern)*                            ('←' program)?
    | 'from' effect_param? ':' pattern                                  ('←' program)?
    (* '←' body is optional: absent = abstract method; present = default implementation *)
    (* source type for 'from; result is Self (or Self T*) *)

sym_hook_kw = 'bop' | 'uop' | 'iter' | 'gen'
            | 'app' | 'project' | 'assign' | 'view'
            (* symbolic/tick hooks only — pat and from have their own sig forms above *)
```

Within a trait signature, `Self` refers to the implementing type. When `Self`
appears applied — `Self T`, `Self K V` — the implementing type must be a type
constructor of matching arity. The compiler infers this from structural position
(consistent with kind inference for `Nat`, `Effect`, etc.): no explicit kind
annotation is required on `Self`. See SEMANTICS.md §3.5 for the substitution
semantics.

**Supertrait constraints**: `type_constraint?` on a `trait_def` declares that
the implementing type must also satisfy another trait. Any type implementing the
child trait must first implement all supertraits; the compiler enforces this at
the `implementation` block. Supertrait methods are visible inside the child
trait's default implementations.

**Default implementations**: a `sig` entry with a `'←' program` body provides
a default. An `implementation` block may omit defaulted methods (inheriting the
default) or provide its own definition (overriding the default). Only abstract
methods (no `'←'` body) are required by `implementation`.

```
// Ground Self: implementing type is a concrete type
trait Num a
  bop +  : a, a → a
  bop -  : a, a → a
  bop *  : a, a → a
  uop -  : a → a                            // negate
  from   : Int → a                          // numeric literal polymorphism

// Constructor Self: implementing type must be a type constructor (Type → Type)
trait Functor f
  iter 'map    : Self t, (t → u) → Self u   // abstract: must be implemented
  iter 'const  : Self t, u → Self u         // default: replace each element
    ← x v → x 'map (_ → v)

// Supertrait chain: Applicative requires Functor; Monad requires Applicative
∀ (a : Functor) ⇒ trait Applicative f
  bop <*> : Self (t → u), Self t → Self u

∀ (a : Applicative) ⇒ trait Monad f
  op >>= : Self t, (t → Self u) → Self u

trait Foldable f
  iter 'fold : Self t, (a, t → a), a → a   // reduce with accumulator
```

**`implementation` blocks** declare that a concrete type fully implements a
trait. All signatures required by the trait must be provided; missing methods
are a compile error at the closing delimiter:

```ebnf
impl_def = 'implementation' type_constraint? upper atomic_type+ '(' impl_method+ ')'

impl_method = hook_kw hook_head '←' program
            | 'pat' upper pattern (',' pattern)* '←' program
```

After `implementation`: the trait name (`upper`) followed by one or more
concrete `atomic_type`s. The types correspond positionally to the trait's type
variable list. Single-type traits supply one concrete type; multi-type traits
supply two or more.

Standalone `hook_def` forms remain valid for non-trait or one-off extensions
— they populate the dispatch table without trait membership or completeness
checking. See SEMANTICS.md §3.5 for the unified semantics.

```
// Single-type trait declaration
trait Num a
  bop + : a, a → a
  bop - : a, a → a
  bop * : a, a → a
  uop - : a → a
  from  : Int → a

// Single-type implementation block: completeness enforced
implementation Num Complex
  bop + ← a b → {re: a.re + b.re, im: a.im + b.im}
  bop - ← a b → {re: a.re - b.re, im: a.im - b.im}
  bop * ← a b → {re: a.re*b.re - a.im*b.im, im: a.re*b.im + a.im*b.re}
  uop - ← a   → {re: 0 - a.re, im: 0 - a.im}
  from Int → Complex ← n → {re: n from Float, im: 0.0}

// Constructor-Self implementation: Self=Tree, so Self T = Tree T, Self U = Tree U
// Compiler kind-checks that Tree : Type → Type matches Self's arity in Functor
implementation Functor Tree
  iter 'map ← t f → t prim.tree_map f

// Multi-type trait declaration
trait Add a b r
  bop + : a, b → r

// Multi-type implementation block
implementation Add Int Float Float
  bop + ← (a b → prim.int_float_add a b)
```

### 9.6 Compile-Time Functions

There is no `macro` keyword. Compile-time computation is expressed using
`Program T` values (§5.5) and ordinary functions:

- `` `[expr] `` produces a `Program T` value.
- `.(f arg)` evaluates `f arg` at compile time and splices the `Program T`
  result into a `T`-typed position.
- Any function `Program A → ... → Program B` is a compile-time transformer
  when its result appears in a splice.

```
// Compile-time function: no special declaration syntax required
liftBinop : Program (a → a → a) → Program a → Program a → Program a
liftBinop ← op l r → `[.(l) .(op) .(r)]

x : Int ← .( liftBinop `[(+)] `[1] `[2] )    // compiles to: x ← 1 + 2
```

See SEMANTICS.md §7 for the full semantics, including the elimination of the
stage restriction by whole-program compilation.

### 9.7 Modules

```ebnf
module_def           = 'module' upper '(' interface_block? implementation_block? ')'
interface_block      = 'interface' '(' program ')'
implementation_block = 'implementation' '(' program ')'

open_stmt   = 'open' module_path open_filter?

module_path = (upper '.')* upper

open_filter = '(' import_list ')'              (* import only these names *)
            | 'hiding' '(' import_list ')'     (* import all except these *)

import_list = import_item (',' import_item)*

import_item = lower             (* value / function name *)
            | '(' op ')'       (* operator: (÷), (+), (++) *)
            | tick              (* modifier: 'map, 'fold *)
            | upper             (* type or module name *)
```

Module bodies use `( )` (consistent with `trait`), or layout form (§1.7).
Inside an explicit `( )`, newlines are statement separators. Each module
contains at most one `interface` block and one `implementation` block.

**Visibility rule**: names declared in `interface ( )` are exported; names
in `implementation ( )` are private. See SEMANTICS.md §8 for the full
semantics.

`open` in an `interface` block re-exports the named module's public names
(subject to any `open_filter`). `open` in an `implementation` block imports
them for internal use only. Qualified access `Module.name` is always available
for public names regardless of `open` statements.

**Selective import and `hiding`**: the `open_filter` mirrors Haskell's import
syntax. `open Foo (÷, ∑)` imports only `÷` and `∑` from `Foo`; `open Foo
hiding (÷)` imports everything from `Foo` except `÷`. Operators are named
using the section form `(op)`.

**`Watch` rule scoping**: a `Watch` attribute (§1.5, §2 Tier 5) is tied to
its definition. When the definition is in scope, its watcher rule is active;
when excluded by a filter, it is not. There is no separate mechanism to import
or suppress watcher rules independently of their definitions.

```
open MyMath                  // import everything, including all Watch rules
open MyMath ((÷), (∑))      // import ÷ and ∑ only; their Watch rules active
open MyMath hiding ((÷))    // import all except ÷; ÷ Watch rule not active
```

```
module Seq
  interface
    data Seq a ← Empty | Cons a (Seq a)
    empty : ∀ a ⇒ Seq a
    cons  : ∀ a ⇒ a → Seq a → Seq a
  implementation
    empty ← Empty
    cons  ← (x, xs) → (x, xs) Cons
```

Abstract (opaque) type — constructors private, interface exports only the
type name:

```
module Map
  interface
    data Map k v                       // no constructors exported
    empty : ∀ k v ⇒ Map k v
    insert : ∀ k v ⇒ Map k v → k → v → Map k v
  implementation
    data Map k v ← Leaf | Node (Map k v) k v (Map k v)
    empty  ← Leaf
    insert ← (m, k, v) → ...
```

**⚑ open**: Functors (parameterised modules) — see SEMANTICS.md §8.

### 9.8 Table Definitions

Tables are not a primitive form separate from arrays and records. A table is
an array of records: `Trade[n]`. Both row-oriented and columnar
representations are already typable using the existing type system:

```
data Trade ← {sym: Symbol, price: Float}   // row schema

// Row-oriented: array of records
trades : Trade[n]

// Columnar: record of arrays (same dimension n ties columns together)
trades : {sym: Symbol[n], price: Float[n]}
```

These are the same logical data in different physical layouts. The `flip`
isomorphism between them is a zero-cost `from` coercion auto-generated by the
compiler for any transparent nominal `type` definition (SEMANTICS.md §2.3). Storage
layout is selected by type annotation, exactly as `HashMap` vs `BTreeMap`:

```
[{sym: `AAPL, price: 100.0}; {sym: `GOOG, price: 200.0}] : Trade[2]
// or equivalently, constructed columnar:
{sym: [`AAPL; `GOOG], price: [100.0; 200.0]} : {sym: Symbol[2], price: Float[2]}
```

### 9.9 Derived View Definitions

A `derived` definition declares an **incrementally-maintained view** over one
or more `⟪State⟫` cells. The compiler ensures the named value is consistent
with its source state after every event handler completes. See SEMANTICS.md
§6.2 for the `Delta` type, incrementalization levels, and debug/release
consistency semantics.

```ebnf
derived_def  = 'derived' lower '←' program
```

The `program` on the right-hand side is an expression over `⟪State⟫` values
using array operations. The compiler analyses which `⟪State⟫` cells are read
and registers the binding for incremental maintenance.

```
// Filter view: maintained O(1) per change (Level 1)
derived activeTrades ← db.trades 'filter (t → t.status = Active)

// Groupby index: bucket insert/remove per delta (Level 1)
derived byEmployer ← db.employments 'groupby .employer

// Chained derived: delta propagates through the chain
derived activeByEmployer ← activeTrades 'groupby .employer

// CMonoid aggregation: heap-backed O(log n) max (Level 2)
derived bestPrice ← db.trades 'map .price 'max ()
```

`derived` definitions appear at module top-level or inside `impl` blocks
scoped to a specific state cell. A `derived` binding may appear in a module
`interface` block as a read-only projection (export semantics: SEMANTICS.md §8).

---

### 9.10 Extern Declarations

An `extern` declaration binds a C symbol by name, or provides an inline C
expression body for macro wrapping. The compiler emits an `Extern` node in
Core (see CORE.md §8.3).

```ebnf
extern_def   = extern_attr* 'extern' '"C"' extern_sig
extern_sig   = lower ':' type                             (* symbol call form *)
             | lower param_list '=' string ':' type       (* expression body form *)
param_list   = '(' (lower ':' type (',' lower ':' type)*)? ')'
extern_attr  = '/\'-Pure-\'/'
             | '/\'-Header' string '-\'/'
```

The `/'-Header "..."-'/` attribute injects an `#include` into the generated C
file, making `static inline` definitions visible to the C compiler at the call
site. Since Arra emits C, the C compiler handles inlining freely across the
boundary — this attribute is how you make that possible for header-only
definitions. See SEMANTICS.md §9.1 for the framing.

All C pointers use `Ptr a`; see SEMANTICS.md §9 for the type mapping.

```
// Effectful — default ⟪IO⟫
extern "C" malloc : Nat → Ptr Byte
extern "C" free   : Ptr Byte → ()
extern "C" fwrite : Ptr Byte, Nat, Ptr File → Nat

// Attribute-block: /'-Header-'/ and /'-Pure-'/ apply to all three
/'-Header "<math.h>"-'/ /'-Pure-'/ (
  extern "C" sin  : Float → Float
  extern "C" cos  : Float → Float
  extern "C" sqrt : Float → Float
)

// Per-declaration when headers differ
/'-Header "<stddef.h>"-'/ extern "C" offsetof_sym (t : Trade) = "offsetof(Trade, sym)" : Nat
/'-Header "<x86intrin.h>"-'/ extern "C" rdtsc : () → Nat
```

`extern` declarations appear at module top-level only. The symbol name (call
form) must match the C ABI name exactly; no name mangling is applied.
Declaring the same symbol at two different types is a type error.

Expression-body argument binding uses hybrid re-binding: see semantics/09_FFI.md §9.5.

---

## 10. Open Questions

