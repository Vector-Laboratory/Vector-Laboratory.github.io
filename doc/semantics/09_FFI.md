# 9. FFI — Foreign Function Interface

## 9.1 Overview

`extern "C"` declarations bind C symbols. The type checker treats them as
ordinary Arra values with the declared type; the compiler emits direct C calls
at each use site. See SYNTAX.md §9.10 for declaration syntax and CORE.md §8.3
for the `Extern` Core node.

**The C-to-C boundary.** Arra's backend emits C. At an `Extern` call site the
emitter produces a C function call (or injects a C expression directly for the
expression-body form). Because the output is already C, the C compiler sees
both the Arra-generated code and the FFI implementation simultaneously — in the
same translation unit if a header is included, or across translation units with
standard LTO. Inlining, constant folding, and all other C compiler
optimizations apply freely across the boundary. This is not a special property
of Arra's FFI; it is a consequence of emitting C.

The practical implication: **call overhead is not a concern Arra needs to
manage**. A `static inline` FFI function defined in a header and included via
`/'-Header "..."-'/` is inlined by the C compiler at every call site with no
special compiler support. The expression-body form (§9.5) exists for the narrow
case of C macros that are not callable as functions at all — not for
performance.

## 9.2 Effect Rules

An `extern "C"` declaration carries effect `⟪IO⟫` by default. The
`/'-Pure-'/` attribute asserts that the C function is pure — no observable side
effects, no global state reads, deterministic output.

This is a **type-system distinction**, not a performance annotation. It
governs where the call may appear in Arra code and how it interacts with hook
dispatch and effect propagation. It has no bearing on whether the C compiler
inlines or optimizes the call.

- Without `/'-Pure-'/`: the call may appear only in `⟪IO⟫`-typed contexts.
- With `/'-Pure-'/`: the call may appear anywhere a pure expression is
  expected. The programmer is responsible for the correctness of the purity
  claim; the compiler performs no checking.

**Fusion opacity:** FFI calls are never fused, moved, or eliminated by Arra's
own fusion passes, regardless of the `/'-Pure-'/` annotation or whether the
extern carries an expression body. `/'-Pure-'/` relaxes the *effect constraint*
only. This rule is unconditional: for call-form externs the C compiler inlines
across the boundary via `static inline`; for expression-body externs the body
is already injected verbatim at each call site by the emitter. In both cases
the C compiler has full visibility and performs all relevant optimizations.
There is nothing Arra's fusion pass could do with the expression string that
the C compiler does not already do.

## 9.3 The `Ptr` Type

`Ptr a` is an opaque pointer type for C interop. Values of type `Ptr a` are
not tracked by the GC; their lifetime is the programmer's responsibility.

- `Ptr Byte` — canonical `void*` analogue
- `Ptr T` for a `data T` declaration — pointer to a GC-managed instance of `T`
- `Ptr (Ptr Byte)` — pointer-to-pointer (e.g., `char**`)

No arithmetic on `Ptr`; `prim.ptr_add : Ptr a, Nat → Ptr a` is provided by
target-specific backend extension for pointer offset arithmetic.

`Ptr` values may not be stored in `⟪State⟫` cells that participate in
`derived` views. The compiler enforces this statically: a `derived` expression
that reads a `Ptr`-typed cell is a type error at the view declaration site.

## 9.4 Type Mapping

Arra to C type correspondence at FFI boundaries:

| Arra type | C type | Notes |
|-----------|--------|-------|
| `Int` | `int64_t` | |
| `Float` | `double` | |
| `Bool` | `bool` (`_Bool`) | |
| `Byte` | `uint8_t` | |
| `Nat` | `size_t` | |
| `()` | `void` | |
| `Ptr a` | `T*` | `a` must be a `type`-defined type or `Byte` |
| `a[n]` | `(T*, size_t)` | passed as pointer + length pair |

Arra's GC-managed values (`String`, arbitrary `type`-defined types, closures) may not
cross FFI boundaries without explicit `Ptr`-boxing. Passing a GC value to C
unboxed is a type error.

Only `Ptr` and pinned-arena-allocated types are valid at FFI call sites.
GC-managed arrays require an explicit `.pin` or `.copy` call at the boundary;
the emitter never pins heap objects implicitly. See BACKEND.md §5 for the
emission rules and BACKEND.md §8 for callback lowering.

## 9.5 Expression-body Externs

An `extern` declaration may carry a C expression string as its body instead
of a symbol name. This exists solely for C macros that cannot be called as
ordinary functions:

```
expect ← /'-Pure-'/ extern "C" (cond : Bool, expected : Int) : Bool
        = "__builtin_expect(!!(cond), expected)"
```

The emitter handles each call site by wrapping in a fresh C block that
re-binds the computed arguments as C locals under the Arra parameter names,
then injects the body string verbatim:

```c
{
    bool    cond     = /* emitter's internal expr for arg 0 */;
    int64_t expected = /* emitter's internal expr for arg 1 */;
    __builtin_expect(!!(cond), expected)
}
```

**Why this binding strategy:** the body is written with Arra parameter names
(readable, matches C macro documentation). The emitter remains free to mangle
its own temporaries as usual — no special carve-out needed for expression-body
externs. Multiple evaluation is impossible since each argument is bound exactly
once. No hygiene risk from emitter-internal naming.

**Static check:** Arra parameter names in an expression-body `extern` must not
be C reserved words (`int`, `return`, `struct`, etc.), enforced at declaration
time.

**Scope restriction:** expression bodies are single C expressions — no local
variables, no statement sequences. Multi-output or stateful patterns belong in
a `static inline` C function called via the call-form `extern`; the call-form
+ `.c` file is the right tool for anything requiring statement-level work.

## 9.6 Header Attributes

`/'-Header "..."-'/` on an `extern` declaration arranges for the named C header
to be included at that declaration's call sites. When many `extern` declarations
share a header, the attribute-block form (SYNTAX.md §1.5) avoids repetition:

```
/'-Header "<math.h>"-'/ /'-Pure-'/ (
  extern "C" sin  : Float → Float
  extern "C" cos  : Float → Float
  extern "C" sqrt : Float → Float
)

// Per-declaration when headers differ — no block needed
/'-Header "<x86intrin.h>"-'/ extern "C" rdtsc : () → Nat
```

**Scoping rule:** a `/'-Header-'/` on the declaration itself overrides the
enclosing attribute-block's `/'-Header-'/` for that declaration. Declarations
without any `/'-Header-'/` (neither inline nor from a block) are emitted
without a header inclusion at call sites — the symbol must be visible through
another included header or provided externally.

