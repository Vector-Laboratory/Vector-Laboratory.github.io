# Arra Semantics

This document is an index. The semantics specification is split into topic
files under `doc/semantics/`:

| Section | File | Contents |
|---------|------|----------|
| §1 | [semantics/01_EXECUTION.md](semantics/01_EXECUTION.md) | Core execution model, word order, whole-program compilation |
| §2 | [semantics/02_TYPES.md](semantics/02_TYPES.md) | Base types, arrays, row types, dependent dimensions, Z3 integration |
| §3 | [semantics/03_HOOKS.md](semantics/03_HOOKS.md) | Hook system, dispatch keywords, specificity, traits, pat hooks |
| §4 | [semantics/04_TRAINS.md](semantics/04_TRAINS.md) | Trains, forks, tacit point-free programming |
| §5 | [semantics/05_EFFECTS.md](semantics/05_EFFECTS.md) | Row-typed effects, handlers, array fusion |
| §6 | [semantics/06_REACTIVE.md](semantics/06_REACTIVE.md) | Reactive state, derived views, incremental maintenance |
| §7 | [semantics/07_METAPROG.md](semantics/07_METAPROG.md) | Quoted programs, compile-time computation, type providers |
| §8 | [semantics/08_MODULES.md](semantics/08_MODULES.md) | Module system, coherence boundaries, orphan rule |
| §9 | [semantics/09_FFI.md](semantics/09_FFI.md) | C FFI, Ptr type, type mapping |
| §10 | [semantics/10_OPEN-QUESTIONS.md](semantics/10_OPEN-QUESTIONS.md) | Open design questions |

## Scope

This specification records semantic design decisions for Arra. It is a precise
record of *how things work*, motivated by the conclusions of design discussions.
Syntax is defined in `../SYNTAX.md`; this document focuses entirely on semantics.
