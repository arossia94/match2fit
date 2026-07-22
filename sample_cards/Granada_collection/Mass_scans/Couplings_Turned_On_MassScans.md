# Granada Collection — Mass-scan cards: couplings turned on (ESPPU26 paper)

## How the mass-scan cards were printed

All cards were generated from `PackageTester.nb` by a single `Do` loop over the model
index (skipping `ii==28` = L1), which for each model reads its flavour file and calls the
printer:

```mathematica
Do[
  If[ii == 28, Continue[]];                         (* skip L1 *)
  condFlaSym = Get[".../Granada_Collection/Flavour_Sym_Cond_SMEFiT_Tree_Level_Mod_" <> ToString[ii] <> "_25_07_2025.dat"];
  matchResToMasScanCard[
     ".../Granada_Collection/Matching_Result_Tree_Level_Mod_" <> ToString[ii] <> "_25_07_2025.dat",
     1,        (* UVcoup = 1  -> every surviving coupling set to 1 *)
     0,        (* looplevel = Tree *)
     {"UVFlavourAssumption" -> Join[realDiagSMYukas, condFlaSym],
      "Collection" -> "Granada", "Model" -> ToString[ii], "OutputFormat" -> "SMEFiT"}];
, {ii, 2, 49}]                                        (* + a re-run {ii,{15,16}} after the antisym fix *)
```

Inside `matchResToMasScanCard` (`match2fit.wl:1361`), the flavour assumption is applied, the
**surviving UV couplings are collected (`varsUV`) and each is set to `UVcoup = 1`**, logs are
dropped, and the Wilson coefficients are written as polynomials in 1/M into
`SMEFiT_runcard_MassScan_Granada_Mod_<n>_UVcoup_1_Tree.yaml`.

So **"couplings turned on" = the components that survive `Join[realDiagSMYukas, condFlaSym]`** —
the global "SM Yukawas real & diagonal" assumption plus the per-model `Flavour_Sym_Cond_*` file.

## Couplings turned on per model (tree level)

| Code | Field | Couplings turned on |
|----|----|----|
| 2 | S | `kS, k3S, lamS` |
| 3 | S₁ | `yS1f12, yS1f21` (antisym.) |
| 4 | S₂ | `yS2f32` |
| 5 | φ | `lamVarphi, yVarphiuf33` *(also done at 1-loop)* |
| 6 | Ξ | `kXi, lamXi` |
| 7 | Ξ₁ | `kXi1, lamXi1, lampriXi1` |
| 8 | Θ₁ | `lamTheta1` |
| 9 | Θ₃ | `lamTheta3` |
| 10 | ω₁ | `yomega1qLf33, yomega1qqf33` |
| 11 | ω₂ | `yomega2f12` → **empty card** (no fit operator) |
| 12 | ω₄ | `yomega4uuf33` |
| 13 | Π₁ | *(all `yPi1f`→0)* → **empty card** |
| 14 | Π₇ | `yPi7Luf33` |
| 15 | ζ | `yZetaqLf33, yZetaqqf33` |
| 16 | Ω₁ | `yOmega1qqf33` |
| 17 | Ω₂ | `yOmega2f21` → **empty card** (no fit operator) |
| 18 | Ω₄ | `yOmega4f33` |
| 19 | Υ | `yUpsf33` |
| 20 | Φ | `yPhiquf33` |
| 21 | 𝓑 | `gBH, gBqf33, gBuf33, gBLf11, gBLf22, gBLf33, gBef11, gBef22, gBef33` |
| 22 | 𝓑₁ | `gB1H` |
| 23 | 𝓦 | `gWH, gWqf33, gWLf11, gWLf22, gWLf33` |
| 24 | 𝓦₁ | `gW1H` |
| 25 | 𝓖 | `gGqf33, gGuf33` |
| 26 | 𝓖₁ | *(all `gG1f`→0)* → **empty card** |
| 27 | 𝓗 | `gHf33` |
| 28 | ℒ₁ | **skipped — no card generated** |
| 29 | ℒ₃ | `gL3f33` |
| 30 | 𝒰₂ | `gUv2Lqf33` |
| 31 | 𝒰₅ | `gUv5edf33` |
| 32 | 𝒬₁ *(vector LQ)* | `gQv1uLf33` |
| 33 | 𝒬₅ *(vector LQ)* | `gQv5eqf33, gQv5uqf33` |
| 34 | 𝒳 | `gXf33` |
| 35 | 𝒴₁ | *(all `gY1f`→0)* → **empty card** |
| 36 | 𝒴₅ | `gY5f33` |
| 37 | N | `lamNef3` |
| 38 | E | `lamEff3` |
| 39 | Δ₁ | `lamDelta1f3` |
| 40 | Δ₃ | `lamDelta3f3` |
| 41 | Σ | `lamSigmaf3` |
| 42 | Σ₁ | `lamSigma1f3` |
| 43 | U | `lamUf3` |
| 44 | D | `lamDff3` |
| 45 | Q₁ *(VLQ)* | `lamQ1uf3` |
| 46 | Q₅ *(VLQ)* | *(all `lamQ5f`→0)* → **empty card** |
| 47 | Q₇ *(VLQ)* | `lamQ7f3` |
| 48 | T₁ *(VLQ)* | `lamT1f3` |
| 49 | T₂ *(VLQ)* | `lamT2f3` |

**Legend for the suffixes:** `...H` = coupling to the Higgs; `y...` = scalar–fermion Yukawa;
`g...` = vector current/gauge coupling; `k`/`lam` (bosons) = cubic/quartic potential couplings;
`lam...f` (37–49) = heavy–light fermion mixing. `f33` = third-generation (3,3) component,
`f3` = third-generation index, `f12`/`f21` = off-diagonal 1st–2nd generation. The pattern is the
SMEFiT flavour scheme: keep the **third-generation-diagonal** coupling(s) plus any
**flavour-blind** Higgs/potential couplings.

## Notes

- **6 models give empty cards** (11 ω₂, 13 Π₁, 17 Ω₂, 26 𝓖₁, 35 𝒴₁, 46 Q₅): their only coupling
  is a single fermionic matrix that the flavour assumption either zeroes entirely (13, 26, 35, 46)
  or reduces to an off-diagonal light-flavour component (ω₂→`yomega2f12`, Ω₂→`yOmega2f21`) that
  produces no operator in the SMEFiT basis. These have no UV-scan card either.
- **Model 28 (L1)** is explicitly skipped in the loop → no card exists.
- **Two naming collisions** to keep in mind: codes 32/33 are the *vector leptoquarks* 𝒬₁/𝒬₅,
  while 45/46 are the *vector-like quarks* Q₁/Q₅ — different fields sharing the label.
