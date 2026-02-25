# 10. Open Questions

The following semantic questions are identified but not yet resolved,
ordered by decreasing difficulty and impact.

- **SOAC fusion — rank-polymorphic horizontal fusion and `'scan` horizontal
  fusion** ([05_EFFECTS.md §5.7](05_EFFECTS.md#57-array-fusion)): vertical fusion rules, the SOAC taxonomy, filter-streaming
  semantics, and simple horizontal fold fusion are now fully specified. Two
  sub-problems remain deferred: (a) **rank-polymorphic horizontal fusion** —
  when a rank-2 `'map` nests rank-1 operations, does the outer loop and inner
  loop fuse, and how does this interact with the producer/consumer graph having
  a rank axis? (b) **`'scan` horizontal fusion** — multiple `'scan` consumers
  sharing the same input require dependency analysis (each scan step depends on
  the previous), making the fusion condition more complex than for folds.
  Both are deferred to implementation experience.

- **Egison-style first-class patterns (reconsideration)**: `pat` hooks
  ([03_HOOKS.md §3.7](03_HOOKS.md#37-pattern-hooks-pat)) are user-extensible patterns but are not first-class values —
  they are named hooks, not values that can be stored, passed, or
  returned. Egison takes the maximally general position: patterns are
  first-class values, and *matchers* (user-defined types that specify how
  pattern forms apply) make the entire matching semantics programmable.
  This enables non-linear patterns, backtracking over infinite structures,
  and patterns composed as ordinary data. For Arra, this would mean a
  `Pat a b` type (a pattern that matches against `a` and extracts `b`),
  combinators `patAnd`, `patOr`, `patMap`, and so on. Deferred: `pat`
  hooks cover the practical cases (view patterns, active patterns,
  Z3-witnessed refinements). A `Pat a b` type could be built as a stdlib
  type using `Program T` ([07_METAPROG.md §7](07_METAPROG.md#7-quoted-programs-and-compile-time-computation)) construction and splicing, without committing
  to it as a language primitive.
