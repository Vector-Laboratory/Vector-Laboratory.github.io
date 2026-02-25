# Loop IR

Loop IR is a structured loop representation that sits between the fusion passes
([BACKEND.md §2](BACKEND.md#2-whole-program-passes)) and C emission ([BACKEND.md §5](BACKEND.md#5-c-emission)). Its purpose is to express SIMD
width parametricity, accumulator register conventions, predicated execution,
tiling, and unrolling as first-class nodes — making these properties explicit
and transformable before any C is emitted.

**Position in the pipeline:**

```
Core IR
  → Whole-program passes (monomorphization, effect annotation, fusion layers 1–3)
  → Kernel selection (§2.7) — matches known patterns to hand-written kernels
  → Loop IR construction    ← this document
  → Loop IR transformations (tiling, vectorization, unrolling)
  → Loop IR lowering        (abstract SIMD width → concrete ISA)
  → C emission
```

Loop IR only receives **pure, fused** operation chains — effect row `e = ⟪⟫`,
all dispatch resolved, all types monomorphic. Effectful operations pass through
unchanged to generic C emission ([BACKEND.md §5](BACKEND.md#5-c-emission)).

The kernel library ([BACKEND.md §6](BACKEND.md#6-primitive-kernel-library)) remains the ground truth for performance.
The long-term goal of Loop IR is to generate code that matches or beats the
hand-written kernels automatically for any fused pattern, not just the patterns
in the initial library. The QCheck property for Loop IR coverage: for every
pattern covered by the kernel library, Loop IR output matches kernel output on
the target hardware.

---

## 1. Loop Nodes

### 1.1 Scalar Loop

The base form. An index loop over a range with a body and an optional
loop-carried accumulator.

```
Loop {
  var   : name           -- loop index variable
  range : LoopRange      -- bounds (see §1.5)
  body  : Stmt           -- loop body; may read var
  acc   : Accum?         -- if present: fold mode (see §1.2)
}
```

A `Loop` without `acc` is a pure mapping loop — one output element per input
element. A `Loop` with `acc` is a fold — the accumulator threads through
iterations and the loop produces a scalar result.

### 1.2 Accumulator

A loop-carried value threading through a fold loop.

```
Accum {
  name : name   -- name bound inside body for the current accumulator value
  init : Expr   -- initial value (before first iteration)
  step : Expr   -- updated value (computed each iteration, becomes next acc)
}
```

Multiple accumulators on a single loop represent horizontally fused folds
([BACKEND.md §2.4](BACKEND.md#24-layer-2--soac-fusion) horizontal fusion). The accumulator tuple is a single register
group; the C emitter assigns one accumulator variable per entry.

### 1.3 Vectorized Loop

A loop whose body operates on SIMD vectors of abstract width `W`. `W` is a
`Nat`-kinded variable resolved at lowering to a concrete ISA width (see §3).

```
VecLoop {
  var   : name           -- loop index (counts in units of W elements)
  range : LoopRange      -- scalar range; emitter generates W-stride iteration
  width : W              -- abstract SIMD width (elements, not bytes)
  body  : VecStmt        -- body over vec<W, T> values
  acc   : VecAccum?      -- optional SIMD accumulator (for map-reduce)
  tail  : Loop?          -- scalar epilogue for remainder elements (n mod W ≠ 0)
}
```

The scalar `tail` handles the remainder loop when `n` is not a multiple of `W`.
If `W` is chosen to divide all reachable `n` statically (e.g. all sizes are
multiples of 8), `tail` may be omitted.

### 1.4 Predicated Loop

A vectorized loop body where some lanes are inactive. Used for fused
`'filter >> 'map` patterns where predicate evaluation produces a lane mask.

```
PredLoop {
  var   : name
  range : LoopRange
  width : W
  pred  : VecExpr        -- evaluates to mask<W, Bool>
  body  : VecStmt        -- executed only on active lanes
  acc   : VecAccum?
  tail  : Loop?
  form  : MaskForm       -- AVX512Mask | BranchPerLane | SelectBlend
}
```

`MaskForm` controls the lowering strategy:
- `AVX512Mask` — uses AVX-512 mask registers; predicate folded directly into
  masked instructions
- `BranchPerLane` — scalar fallback; branches on each lane result
- `SelectBlend` — blend/select instruction; predicate applied via `_mm_blendv`

Lowering selects `MaskForm` from the target capability flags. `AVX512Mask` is
preferred when available; `SelectBlend` otherwise; `BranchPerLane` only as a
last resort for non-SIMD targets.

### 1.5 Loop Range

A loop range is either a closed integer interval or a Range-typed descriptor
(from `Range Int` source arrays — see [semantics/02_TYPES.md §2.1](semantics/02_TYPES.md#21-base-types-and-array-types)). Range
sources map directly to loop bounds without allocation.

```
LoopRange
  = Interval { lo: Expr, hi: Expr }          -- [lo, hi)
  | RangeDesc { start: Expr, end: Expr, step: Expr }  -- start..end..step
```

### 1.6 Zip

Parallel iteration over multiple arrays. Used for `'zipWith` and for SoA
(struct-of-arrays) record arrays where a single logical `'map` decomposes
into parallel field streams.

```
Zip {
  streams : (name × ArrayRef)+  -- each name bound to one stream element
  range   : LoopRange           -- shared iteration range; all streams same length
  body    : Stmt
  acc     : Accum?
}
```

A SoA `'map` over `{x: Float, y: Float}[n]` becomes a `Zip` over the `x` and
`y` field arrays. No AoS intermediate is allocated.

---

## 2. Transformations

Transformations rewrite Loop IR nodes into other Loop IR nodes. They are
applied before lowering (§3) and after Loop IR construction.

### 2.1 Tiling

Splits a loop into an outer tile loop and an inner lane loop. The tile size `T`
is a `Nat`-kinded parameter chosen per target at lowering time.

```
tile(T) :
  Loop { var i; range [0, n); body B }
  →
  Loop { var tile; range [0, n/T); body:
    Loop { var lane; range [tile*T, tile*T + T); body B[i := lane] }
  }
```

The inner loop body is the vectorization target. `T` is typically chosen to
equal the SIMD width `W` for the target ISA, but may be larger for cache-tiling.

Tiling is a prerequisite for vectorization: `vectorize(W)` (§2.2) applies to
the inner loop of a tiled pair.

### 2.2 Vectorization

Lifts the inner loop of a tiled pair into a `VecLoop` with abstract width `W`.

```
vectorize(W) :
  Loop { var i; range [lo, lo+W); body B }
  → VecLoop { var i; range ...; width W; body lift(B, W) }
```

`lift(B, W)` rewrites scalar operations in `B` to their `vec<W, T>` equivalents.
Every primitive operation in the body must have a vector form; if any does not,
vectorization fails and the loop falls through to scalar emission.

### 2.3 Unrolling

Replicates the loop body `U` times and adjusts the bounds accordingly.
Applied to the scalar tail (§1.3) or to small fixed-bound loops.

```
unroll(U) :
  Loop { var i; range [0, n); body B }
  →  B[i:=0]; B[i:=1]; ...; B[i:=U-1];  Loop { var i; range [U, n); body B }
```

Full unrolling (when `n` is a compile-time constant ≤ threshold) emits the body
`n` times with no loop overhead.

---

## 3. Lowering

Lowering resolves abstract SIMD widths `W` to concrete ISA widths and selects
`MaskForm` for predicated loops based on runtime-detected target capabilities.

The target capability set is determined once at program startup (CPUID on x86,
HWCAP on ARM) and stored as a constant for the compilation. Lowering is
**not** a runtime dispatch — the Loop IR is lowered to a specific concrete
width for each compilation target. Runtime dispatch between SIMD code paths
happens at the kernel library level ([BACKEND.md §6](BACKEND.md#6-primitive-kernel-library)), not within a single Loop
IR lowering.

**Width resolution table:**

| Target | `W` for Float | `W` for Int32 | `W` for Bool (bit-packed) |
|--------|--------------|--------------|--------------------------|
| SSE2   | 4            | 4            | 128                       |
| AVX2   | 8            | 8            | 256                       |
| AVX-512| 16           | 16           | 512                       |
| NEON   | 4            | 4            | 128                       |

Bool (bit-packed) arrays use a separate lane model: one SIMD register holds
`W_reg` bits; bitwise operations apply to the full register.

---

## 4. Relationship to the Kernel Library

The kernel library ([BACKEND.md §6](BACKEND.md#6-primitive-kernel-library)) is the **ground truth** for Loop IR
correctness and performance. Every fused pattern covered by the kernel library
is also a target for Loop IR:

| Kernel pattern | Loop IR representation |
|---|---|
| `'map f` | `VecLoop` (no acc) |
| `'fold g` | `VecLoop` with `VecAccum` + horizontal reduction epilogue |
| `'map f >> 'fold g` | `VecLoop` with fused body + `VecAccum` |
| `'filter p >> 'map f` | `PredLoop` |
| `'map f >> 'map g` | `VecLoop` with composed body (from Layer 1 map fusion) |
| `'zip >> 'map f` | `Zip` inside `VecLoop` |

**Validation:** the QCheck harness ([BACKEND.md §7](BACKEND.md#7-qcheck-harness)) gains a new property suite
for Loop IR: for every kernel-covered pattern, Loop IR lowered output must
match kernel output across element types, array sizes, and SIMD code paths.
Kernel output is the oracle; Loop IR output is the subject.

---

## 5. Open Questions

- **Tile size selection.** `T` is described as a lowering-time parameter, but
  the right `T` depends on cache topology (L1/L2 line sizes, associativity).
  Options: fixed per-ISA constants, profile-guided selection, or a
  target-description record passed through lowering. Deferred.

- **Horizontal reduction epilogue.** A `VecLoop` fold produces a SIMD
  accumulator register; the final scalar result requires a horizontal reduction
  (e.g. `_mm256_hadd_ps` or a manual shuffle sequence). This epilogue is
  pattern-specific and ISA-specific. Should it be a first-class Loop IR node,
  or a lowering-level emission detail?

- **Multi-output folds (horizontal fusion).** A horizontally fused fold has a
  tuple accumulator. The Loop IR `Accum` list models this, but the lowering
  must assign each accumulator to a register or register group without spilling.
  Register pressure analysis is needed for large tuple accumulators.

- **Rank > 1.** All nodes above assume rank-1 arrays. Rank-2 and higher require
  nested `Loop`/`VecLoop` nodes with multi-dimensional tiling. The interaction
  between the outer and inner tile loops, and how the existing `'rank`-based
  dispatch integrates, is unspecified.

- **`'scan` lowering.** `'scan` (prefix reduction) has a loop-carried dependency
  between iterations that prevents straightforward vectorization. Parallel
  prefix algorithms exist but require a different loop structure. Deferred.
