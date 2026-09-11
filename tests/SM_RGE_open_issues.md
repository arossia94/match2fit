# SM RGE evolution — open issues

Working notes for the `add_SM_RGEs` branch. Every issue below was reproduced on a
Wolfram 14.2 kernel against the sample results in `sample_results/`.

## Baseline

```
wolframscript -file tests/check_SMRGE_matching_scale.wls
```

Currently **47 passed, 5 failed**. The five failures are issues 2, 4 and 5 below.
Issue 3 is not covered by the check yet.

Already fixed on the branch:
- the matching scale is substituted at every loop order (`massHandler` /
  `massHandlerCusto`, [`match2fit.wl:253`](../match2fit.wl#L253),
  [`:260`](../match2fit.wl#L260), [`:278`](../match2fit.wl#L278),
  [`:286`](../match2fit.wl#L286));
- `flavourSymChecker` is exported again ([`:29`](../match2fit.wl#L29));
- issue 1a — mass-scan cards no longer print `Indeterminate`
  ([`:1489-1501`](../match2fit.wl#L1489-L1501));
- issue 1b — given a temporary, intermediate treatment pending the new SMEFiT
  output format: SM RGEs off by default for mass-scan cards, opt-in evaluation
  at a user-supplied `"MaxMass"` otherwise ([`:1483`](../match2fit.wl#L1483),
  [`:1488-1497`](../match2fit.wl#L1488-L1497),
  [`:1560`](../match2fit.wl#L1560), and the `Options[...]`/call-site plumbing
  in `matchResToMasScanCard` and `modelToMasScanCard`).

Section 6 of the check now covers both (18/18 passing): no `Indeterminate`, no
residual μ, no unevaluated `InterpolatingFunction`, the heavy mass stays free in
both modes, `MaxMass->None` leaves the global SM RGE setting untouched, and
`MaxMass->10` genuinely differs numerically from the RGE-forced-off default.

---

## 1. Mass-scan cards are corrupted when the RGEs are on — **highest severity**

`dictPrinterUVmass` was writing `Indeterminate` into the run card. Two independent
causes, both on the same path, both addressed.

### 1a. `Log[a_] :> 0` destroys the interpolant's own argument — **fixed**

[`match2fit.wl:1501`](../match2fit.wl#L1501) forces the explicit matching logs to zero:

```mathematica
dicTotal=dicTotal/.{Log[a_]:>0};
```

With the RGEs on, the SM couplings are `InterpolatingFunction[...][Log[1000 μ]/Log[10]]`.
That rule rewrote the *argument* as well — both `Log[1000 μ]` and the `Log[10]` in the
denominator match `Log[a_]` — giving `0/0`, so the kernel emitted `Power::infy` /
`Infinity::indet` and the WC values came out `Indeterminate`. The
`InterpolatingFunction[...][Indeterminate]` objects then reached `Variables[]` at
[`:1503`](../match2fit.wl#L1503) and were assigned the numeric UV coupling shortly after,
i.e. printed as fit parameters.

Reproduced with `Q1_U` at tree level: 3 `Indeterminate` values. Independent of the
loop level and independent of the matching-scale fix already applied.

**Fix applied** ([`:1498-1499`](../match2fit.wl#L1498-L1499)): substitute the μ rule
from `massReemp` and force `N[]` specifically inside any `InterpolatingFunction[...][...]`
call — `//.hf_InterpolatingFunction[arg_] :> hf[N[arg]]` — *before* the log-killing rule
runs, rather than after. Only the interpolant's own argument is forced numeric; the
heavy-mass symbols are left untouched (they are the scan variable and must survive into
the printed card as `1/invM` polynomials).

A first attempt wrapped the whole dictionary in `N[]` instead of targeting the
`InterpolatingFunction` calls specifically; that also silently turned generation
indices like `L2[2]` into `L2[2.]` (`Integer` → `Real`), which would have broken
matching against declared UV coupling names downstream. Caught before landing by
checking `Head /@ (indices)` after the fix — worth remembering as a shape for how
this class of bug hides.

### 1b. The matching scale is frozen at an arbitrary placeholder — **intermediate fix applied**

Not just a bug — a genuine representational gap. Physically, μ must equal the (single)
heavy mass for the fixed-order matching logs to vanish, which is exactly what
[`:1501`](../match2fit.wl#L1501)'s `Log[a_] :> 0` encodes. For a mass scan that means μ
must equal the *scan variable itself* — a symbol, not a number. But `matchResToMasScanCard`
never receives a mass range at all (SMEFiT decides which numeric masses to scan later,
downstream of card generation), and the SMEFiT mass-scan format can only express a WC as a
polynomial in `1/M` with fixed numeric coefficients — it has no way to encode "this
coefficient is itself a transcendental function of `M`" (the RGE-evolved SM couplings are
an `InterpolatingFunction`, not a polynomial). So evaluating the running couplings at any
fixed numeric μ is a real approximation, not an implementation detail to be perfected away;
freezing μ at an arbitrary placeholder value (`1`, from the structural placeholder mass
`massHandler[matchResFile,1,...]` was always called with) was simply the *wrong* choice of
approximation, with nothing behind it.

**Intermediate solution applied**, pending a SMEFiT output format that can express a
μ-dependent coefficient (a proper fix needs a new card format, not a code change here):
- **By default** (`"MaxMass"->None`), the SM RGE evolution is switched off for mass-scan
  cards specifically, regardless of the global `setSMRGELoop`/`setSMRGEintegration`
  setting — done via `Block[{$loopLevelSMRGE=0,$integLevelSMRGE="None"}, massHandler[...]]`
  ([`:1491`](../match2fit.wl#L1491)), which temporarily overrides the two `Protected`
  globals for just that call and is guaranteed to restore them afterward (verified: global
  state unchanged before/after a default-mode call even under `Block`, no `Unprotect`
  leakage). The printed mass upper bound stays the legacy `300`.
- **If the caller opts in** with `"MaxMass"->value`, that value is used as the placeholder
  mass passed to `massHandler` (so μ resolves to it via the existing `massReemp` mechanism)
  *and* becomes the printed mass upper bound ([`:1560`](../match2fit.wl#L1560)). If the SM
  RGEs are on globally, the running couplings are evaluated once at μ = `MaxMass` for the
  whole card. A warning is printed either way (`:1490` for the default, `:1494` for the
  opt-in with RGEs on), stating explicitly that this is an approximation and can be poor for
  wide scan ranges or wherever the true scanned mass sits well below `MaxMass`.

Plumbed through `dictPrinterUVmass` ([`:1483`](../match2fit.wl#L1483), new last parameter),
`matchResToMasScanCard` and `modelToMasScanCard` (new `"MaxMass"` option on both, default
`None`; usage strings at [`:27`](../match2fit.wl#L27) and [`:37`](../match2fit.wl#L37)
document the whole contract, including the "temporary" framing).

Verified end to end by calling the real, file-writing `matchResToMasScanCard` (with
`NotebookDirectory[]` redirected to a scratch directory, since it is `$Failed` headlessly)
and inspecting the actual generated YAML: default card carries `max: 300` and RGE-off
values; `"MaxMass"->10` carries `max: 10` and numerically different values (RGEs on);
`"MaxMass"->10` with RGEs off globally reproduces plain tree-level values. No
`Indeterminate` in any of the three.

Covered by check section 6 (`mass-scan, tree-level` / `mass-scan, one-loop`, each in both
`default` and `MaxMass->10` modes, 18/18 assertions passing): no residual μ, no unevaluated
`InterpolatingFunction`, no `Indeterminate`/`ComplexInfinity`, the heavy mass confirmed
still free in both modes, the global RGE setting confirmed untouched by a default-mode
call, and `MaxMass->10` confirmed to numerically differ from the RGE-forced-off default
(guards against either override silently not taking effect).

**Still to do when the real fix lands:** the `matching_Result_..._generalMu.wxf` results
in the repo root suggest a general-μ form was already being explored — worth checking
whether that is the intended route for the eventual SMEFiT output format that removes the
need for this approximation entirely.

---

## 2. Conjugate Yukawas are not substituted when the RGEs are on

`smCoupsAtMatchingMu` ([`match2fit.wl:404-411`](../match2fit.wl#L404-L411)) supplies rules
for `g1, g2, g3, yu, yd, yl, lam, muH` but omits `yubar, ydbar, ylbar`, which the
RGEs-off branch at [`:432`](../match2fit.wl#L432) does supply.

So with the RGEs on, `yubar[3,3]`, `ydbar[3,3]`, `ylbar[3,3]` stay symbolic, survive
into the dictionary and are printed as UV fit parameters by
[`:1365`](../match2fit.wl#L1365).

Hits the one-loop path (the tree-level results carry no conjugate Yukawas). Reproduced
with `S_oneloop` at 1 loop, `mass = {3}`: free symbols are `{L1, L2, L3, L4, ydbar,
ylbar, yubar}` against `{L1, L2, L3, L4}` with the RGEs off.

**Fix direction:** add the three conjugate rules alongside the others. Since the
Yukawas are real and diagonal in the current setup, `Conjugate[...]` of the evolved
value is the obvious form, but check against what `runSMCouplings` returns for the
off-diagonal entries before assuming it.

**Check assertions:** section 3, "one-loop, single mass: RGEs on and off leave the same
symbols free" and "no non-UV symbol would reach the run card".

---

## 3. `yu`/`yd`/`yl` keep the tree-level values off the diagonal

Same rule list, [`match2fit.wl:407-409`](../match2fit.wl#L407-L409):

```mathematica
Symbol[SymbolName[yu]][a_,b_] -> If[a==3&&b==3, sol[Gu[3,3]], y[u][a,b]]
```

Only the 3-3 entry is taken from the running; every other entry falls back to the
unevolved `y[u][a,b]`. The first two generations are therefore evaluated at m_Z while
the third is evaluated at the matching scale.

Numerically tiny given the light-quark Yukawas, but it is an inconsistency in the
inputs rather than an approximation anyone chose, and the initial conditions at
[`:382-399`](../match2fit.wl#L382-L399) do already seed all nine entries of each matrix.

**Not covered by the check.** Worth an assertion that every SM parameter fed to the
matching result comes from the same scale.

---

## 4. `setSMRGELoop` does not change the running

`ValueQ[SMEFTLoopOrder]` is `False` — the symbol is defined nowhere in the repo. So in
[`src/tools.m:33`](../src/tools.m#L33):

```mathematica
RGEsSMt=...Import[...]/.ToLoopOrder[SMEFTLoopOrder]//Chop;
```

the guard `i > SMEFTLoopOrder` in `ToLoopOrder` ([`src/tools.m:22`](../src/tools.m#L22))
never evaluates to `True`, only `LoopParameter -> 1` fires, and **every** loop order
survives (up to 5 in g_s). The running is always at full order whatever the user asks
for. Confirmed: g3(3 TeV) is bit-identical at "1-loop" and "3-loop", `1.00329`.

Truncating downstream cannot recover it either — `filteredEqs` at
[`match2fit.wl:370`](../match2fit.wl#L370) is computed and never used, and by then
`LoopParameter` has already been flattened to 1, so `ToLoopOrder` on `RGEsSMt` is a
no-op. `SMRunRGEs` ([`src/tools.m:44-52`](../src/tools.m#L44-L52)) integrates the global
`RGEsSMt` and ignores its caller's intent entirely.

**Fix direction:** either set `SMEFTLoopOrder` from `$loopLevelSMRGE` before calling
`BuildRGEs["SM"]` and rebuild whenever `setSMRGELoop` is called, or stop flattening
`LoopParameter` in `BuildRGEs` and let `runSMCouplings` do the truncation — in which
case `SMRunRGEs` has to be given the filtered equations instead of reading the global.
The second is cleaner but touches the vendored DsixTools copy.

**Check assertion:** section 5, "setSMRGELoop changes g3(3 TeV)".

---

## 5. `"leadinglog"` and `"integrate"` are the same thing

`runSMCouplings` never reads `$integLevelSMRGE`. The only place it is consulted is
[`match2fit.wl:431`](../match2fit.wl#L431), and only to test for `"None"`. Both modes
run the full `NDSolve` — identical g3(3 TeV).

A leading-log mode is the linearised solution
`c(t) = c(t_EW) + β(c(t_EW)) · Log[10] · (t − t_EW)/(16π²)`; `SMEFTRunRGEs` in
[`src/tools.m:63-69`](../src/tools.m#L63-L69) already implements exactly that shape for
`RGEsMethod == 2` and can be copied.

**Fix direction:** implement it, or drop the option and the setter rather than shipping
a knob that does nothing.

**Check assertion:** section 5, "setSMRGEintegration changes g3(3 TeV)".

---

## 6. The 500 TeV endpoint is unguarded

[`match2fit.wl:402`](../match2fit.wl#L402) integrates to a hardcoded
`Log10[10^3*500]`, i.e. 500 TeV. Integrating once to a fixed bound and reading the
interpolant at μ is a good design — one solve serves any matching scale — but nothing
checks that the matching scale lands inside `[m_Z, 500 TeV]`.

At μ = 1000 TeV the `InterpolatingFunction` extrapolates silently: g3 = 0.8117 against
0.8289 at the endpoint. Same downward for μ < m_Z.

**Fix direction:** range-check the matching scale in `massHandler` (or in
`runSMCouplings`) and fail loudly. Raising the endpoint alone is not enough — it just
moves the cliff.

**Check assertion:** section 4, "a matching scale above the endpoint is rejected rather
than extrapolated".

---

## 7. Minor / not blocking

- **`massHandlerCusto` is a near-duplicate of `massHandler`** — the two differ only in
  a comment and still carry `(*/// New function to be modified and tested. ///*)` at
  [`match2fit.wl:263`](../match2fit.wl#L263). Every fix above has to be applied twice
  because of it, including the one already made. Worth collapsing.
- **Symbol resolution depends on the caller's context.** `Symbol[SymbolName[\[Mu]]]` is
  evaluated at call time, so the μ that `massHandler` builds and the μ that
  `runSMCouplings` builds are whatever `$Context` is then. Self-consistent within a
  session, but it means the package cannot be driven from a non-`Global`` context
  without the substitutions silently missing. Not currently failing — flagged only so
  it is a deliberate choice.
