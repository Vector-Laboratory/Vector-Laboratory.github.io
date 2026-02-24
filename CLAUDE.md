# CLAUDE.md — Arra Language Spec

**Arra** is a functional array language being designed from scratch. This repo
(`gh-pages` branch) hosts the spec as static docs in `doc/`. There is no
implementation yet — all work is spec authoring.

## Doc Map

| File | Contents |
|------|----------|
| `doc/OVERVIEW.md` | Language motivation and hook keyword table |
| `doc/SYNTAX.md` | Grammar, layout rule, all surface forms |
| `doc/SEMANTICS.md` | Typing rules, effect system, FFI |
| `doc/CORE.md` | Core IR (consumed by both backends) |
| `doc/FRONTEND.md` | Parser → type checker → Core pipeline |
| `doc/BACKEND.md` | Compilation strategy, SOAC fusion, C emission |
| `doc/ROADMAP.md` | Implementation roadmap |

## Design Core

Everything in Arra resolves through the **hook system**: a table of 11 keywords
(`uop`, `bop`, `app`, `pat`, `project`, `assign`, `view`, `iter`, `gen`,
`from`, `iso`), each naming a syntactic dispatch position. Hooks are not
methods — they resolve statically at the import boundary of the containing
module via a specificity ordering. No runtime dispatch tables exist. Three
keywords (`gen`, `from`, `iso`) dispatch on the **result type**; the rest
dispatch on input type.

All major systems interact with hooks: algebraic effects (row-typed, propagate
through hooks automatically), sigma/dimension types (checked per-definition by
Z3), compile-time splice evaluation (`Program T`), and SOAC fusion (the SOAC
marker prims are fusion targets, not implementations). See OVERVIEW.md for the
keyword table and optic-hierarchy rationale; the other docs elaborate each
system in full.

## Notes

- **LTR / equal precedence**: All operators have equal precedence; evaluation
  is strictly left-to-right. `a + b * c` = `(a + b) * c`. See SYNTAX.md §6.
- **Argument-then-function**: Juxtaposition is Forth-style — `f g` means
  `g(f)`. Chains are pipelines: `x f g h` = `h(g(f(x)))`. See SYNTAX.md §6.
- **Pattern branches are unordered**: Match branches must be pairwise
  disjoint (Z3-verified); the compiler may evaluate in any order. This is not
  first-match semantics. See semantics/03_HOOKS.md §3, FRONTEND.md §2.4.
- **C-to-C FFI**: Arra emits C, so the C compiler sees both sides of every
  extern call — no FFI overhead concern at the language level. See
  semantics/09_FFI.md §9, BACKEND.md §8.
- **Layout rule**: Haskell-style indentation with several subtle cases
  (same-line vs next-line `(`, `iso` blocks, `'handle` blocks). Read
  SYNTAX.md §1.7 carefully before editing layout-sensitive sections.
