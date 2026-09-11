#!/usr/bin/env wolframscript
(* ::Package:: *)

(*******************************************************************************
  check_SMRGE_matching_scale.wls

  Regression check for the SM RGE evolution added in the add_SM_RGEs branch.

  What it verifies
  ----------------
  When the SM parameters are evolved with the SM RGEs, the running must be
  evaluated *at the matching scale* of the model being matched, for every
  supported combination of

        {tree-level, one-loop} x {single heavy mass, several heavy masses}

  The governing invariant is:

        switching the SM RGEs on may change the *values* of the SM parameters
        folded into the matching result, but it must not change *which* symbols
        are left free in it.

  Anything still symbolic in the renormalisation scale \[Mu] - or any unevaluated
  InterpolatingFunction - is picked up by Variables[] in dictPrinterUVcoup and
  printed into the run card as if it were a UV fit parameter.

  Sections
  --------
    1. Units and boundary behaviour of the running (TeV <-> GeV, Log10 conventions)
    2. Matching-scale plumbing: does massHandler substitute \[Mu] -> matching scale?
    3. End to end: the RGEs-on dictionary has the same free symbols as RGEs-off
    4. Matching scale vs. the endpoint the RGEs were actually integrated to
    5. Do setSMRGELoop / setSMRGEintegration actually change the running?
    6. Mass-scan cards (dictPrinterUVmass): no Indeterminate leaks into the card

  Usage
  -----
    wolframscript -file tests/check_SMRGE_matching_scale.wls

  Exits with status 0 if every check passes, 1 otherwise.
*******************************************************************************)

(* ---------------------------------------------------------------- harness -- *)

$repoDir = ParentDirectory[DirectoryName[$InputFileName]];
SetDirectory[$repoDir];

$results = {};

report[section_, label_, pass_, detail_: ""] := (
  AppendTo[$results, {section, label, pass}];
  Print[If[TrueQ[pass], "  [PASS] ", "  [FAIL] "], label];
  If[detail =!= "", Print["          ", detail]];
);

info[detail_] := Print["  [info] ", detail];

section[title_] := Print["\n=== ", title, " ==="];

(* Symbols are created at run time in the caller's context, so locate them by
   name rather than by an assumed context. *)
muSymbolsIn[expr_] :=
  DeleteDuplicates@Cases[expr, s_Symbol /; SymbolName[s] === "\[Mu]",
                         {0, Infinity}, Heads -> True];

interpFunctionsIn[expr_] :=
  Cases[expr, _InterpolatingFunction, {0, Infinity}, Heads -> True];

(* Free symbols, reduced to their heads: yubar[3,3] -> yubar. *)
freeHeads[expr_] :=
  Sort@DeleteDuplicates@Replace[Variables[expr], a_[b__] :> a, 1];

(* The matching scale massHandler is expected to use. *)
expectedMatchingScale[mass_] :=
  Which[
    ListQ[mass] && Length[mass] > 1, Min[mass],
    ListQ[mass],                     First[mass],
    True,                            mass
  ];

relDiff[a_, b_] := Abs[a - b] / Max[Abs[a], Abs[b], $MinMachineNumber];

(* --------------------------------------------------------------- load-up -- *)

Print["Loading match2fit from ", $repoDir, " ..."];
Get[FileNameJoin[{$repoDir, "match2fit.wl"}]];

$rgeLoopOrder = 2;

rgesOn[]  := (setSMRGELoop[$rgeLoopOrder]; setSMRGEintegration["integrate"];);
rgesOff[] := (setSMRGEintegration["None"]; setSMRGELoop[0];);

rgesOn[];
Print["\nSM RGE settings: loop order = ", statusSMRGE[],
      ", integration = ", integrationSMRGE[]];

(* SM couplings, as folded into the matching result, at a given matching scale
   \[Mu] (in TeV).  This is exactly the object dictionaryToPrint substitutes. *)
smRepAt[muTeV_] :=
  Module[{rep, mu},
    rep = replaceSMparamsMatchMakerEFT;
    mu  = muSymbolsIn[rep];
    If[mu === {}, rep, rep /. (# -> muTeV & /@ mu)]
  ];

smCouplingAt[name_String, muTeV_] :=
  Module[{rep, r},
    rep = smRepAt[muTeV];
    r   = SelectFirst[rep,
            With[{lhs = #[[1]]},
              SymbolName[If[Head[lhs] === Symbol, lhs, Head[lhs]]] === name] &];
    If[MissingQ[r], $Failed, N[r[[2]]]]
  ];

(* ============================================================== SECTION 1 == *)

section["1. Units and boundary behaviour of the running"];

Module[{rep, ifs, domain, mZTeV, g3AtMZ, g3Ref, g1Lo, g1Hi, g3Lo, g3Hi, alphaS1TeV},

  rep = replaceSMparamsMatchMakerEFT;
  ifs = interpFunctionsIn[rep];

  If[ifs === {},
    report[1, "Running produces InterpolatingFunctions", False,
           "replaceSMparamsMatchMakerEFT returned no InterpolatingFunction - "<>
           "is the RGE evolution actually switched on?"];
    ,
    domain = First[ifs][[1]];
    report[1, "Running produces InterpolatingFunctions", True,
           "integration domain in t = Log10[\[Mu]/GeV]: " <> ToString[domain]];

    (* Boundary: at \[Mu] = mZ the evolved couplings must reproduce the inputs.
       This is the check that the TeV -> GeV conversion Log10[10^3 \[Mu]] is right. *)
    mZTeV  = 10^(First[First[domain]]) / 10^3;
    g3AtMZ = smCouplingAt["g3", mZTeV];
    g3Ref  = N[3 Sqrt[131 Pi]/50];   (* the package's own g3(mZ) input *)
    report[1, "g3 at the low end of the running reproduces g3(mZ)",
           NumericQ[g3AtMZ] && relDiff[g3AtMZ, g3Ref] < 10^-6,
           "\[Mu] = " <> ToString[mZTeV] <> " TeV -> g3 = " <> ToString[g3AtMZ] <>
           "   (input g3(mZ) = " <> ToString[g3Ref] <> ")"];
  ];

  g1Lo = smCouplingAt["g1", 1];   g1Hi = smCouplingAt["g1", 10];
  g3Lo = smCouplingAt["g3", 1];   g3Hi = smCouplingAt["g3", 10];

  report[1, "g1 grows and g3 falls between 1 and 10 TeV",
         NumericQ[g1Lo] && NumericQ[g3Lo] && g1Hi > g1Lo && g3Hi < g3Lo,
         "g1: " <> ToString[g1Lo] <> " -> " <> ToString[g1Hi] <>
         " ; g3: " <> ToString[g3Lo] <> " -> " <> ToString[g3Hi]];

  alphaS1TeV = If[NumericQ[g3Lo], g3Lo^2/(4 Pi), $Failed];
  report[1, "alpha_s(1 TeV) lands in a physically sane band [0.075, 0.095]",
         NumericQ[alphaS1TeV] && 0.075 < alphaS1TeV < 0.095,
         "alpha_s(1 TeV) = " <> ToString[alphaS1TeV]];
];

(* ============================================================== SECTION 2 == *)

(* Q1_U rather than S is used for the tree-level single-mass case because its
   tree-level matching result actually contains SM couplings; S's does not, so
   it cannot see a leak at all. *)
$scenarios = {
  <|"tag"  -> "tree-level, single mass",
    "file" -> "sample_results/MatchingResult_Q1_U.dat",
    "mass" -> {3}, "loop" -> 0|>,
  <|"tag"  -> "one-loop, single mass",
    "file" -> "sample_results/MatchingResult_S_oneloop.dat",
    "mass" -> {3}, "loop" -> 1|>,
  <|"tag"  -> "tree-level, several masses",
    "file" -> "sample_results/MatchingResult_multiparticle_treelevel.dat",
    "mass" -> {2, 5, 8}, "loop" -> 0|>,
  <|"tag"  -> "one-loop, several masses",
    "file" -> "sample_results/MatchingResult_EW_Quad_Y_12_32_Custo.dat",
    "mass" -> {4, 6}, "loop" -> 1|>
};

section["2. Matching-scale plumbing (massHandler)"];

$onDict = <||>; $offDict = <||>; $massReemp = <||>;

Do[
  Module[{sc = $scenarios[[i]], t0, out},
    t0 = AbsoluteTime[];
    rgesOn[];
    out = match2fit`Private`massHandler[sc["file"], sc["mass"], sc["loop"], {}];
    $onDict[i]    = out[[1]];
    $massReemp[i] = out[[3]];
    rgesOff[];
    $offDict[i] =
      match2fit`Private`massHandler[sc["file"], sc["mass"], sc["loop"], {}][[1]];
    Print["  [", sc["tag"], "]  built both dictionaries in ",
          Round[AbsoluteTime[] - t0, 0.1], " s"];
  ],
  {i, Length[$scenarios]}];

rgesOn[];

Do[
  Module[{sc = $scenarios[[i]], massReemp, muRules, expected},
    massReemp = $massReemp[i];
    expected  = expectedMatchingScale[sc["mass"]];
    muRules   = Select[massReemp, SymbolName[#[[1]]] === "\[Mu]" &];

    report[2, sc["tag"] <> ": massReemp carries a rule for \[Mu]",
           Length[muRules] === 1,
           "massReemp = " <> ToString[massReemp]];

    If[Length[muRules] === 1,
      report[2, sc["tag"] <> ": \[Mu] is set to the matching scale " <> ToString[expected],
             muRules[[1, 2]] === expected,
             "\[Mu] -> " <> ToString[muRules[[1, 2]]]];
      ,
      report[2, sc["tag"] <> ": \[Mu] is set to the matching scale " <> ToString[expected],
             False,
             "no \[Mu] rule was produced, so the matching scale is never applied"];
    ];
  ],
  {i, Length[$scenarios]}];

(* ============================================================== SECTION 3 == *)

section["3. End to end: switching the RGEs on must not add free symbols"];

Do[
  Module[{sc = $scenarios[[i]], massReemp, onApplied, offApplied,
          leftoverMu, leftoverIF, headsOn, headsOff, declaredUV, extra, nIF},
    massReemp  = $massReemp[i];
    onApplied  = $onDict[i][[All, 2]]  /. massReemp;
    offApplied = $offDict[i][[All, 2]] /. massReemp;

    nIF = Length[interpFunctionsIn[$onDict[i][[All, 2]]]];
    If[nIF === 0,
      info[sc["tag"] <>
           ": no SM coupling enters this matching result, so this scenario "<>
           "cannot detect a leak (checks below are vacuous)"]];

    leftoverMu = muSymbolsIn[onApplied];
    leftoverIF = interpFunctionsIn[onApplied];

    report[3, sc["tag"] <> ": no residual \[Mu] in the dictionary",
           leftoverMu === {},
           If[leftoverMu === {}, "",
              "leftover: " <> ToString[leftoverMu] <>
              "  -- the SM couplings are still symbolic in the matching scale"]];

    report[3, sc["tag"] <> ": no unevaluated InterpolatingFunction in the dictionary",
           leftoverIF === {},
           If[leftoverIF === {}, "",
              ToString[Length[leftoverIF]] <>
              " InterpolatingFunction(s) left unevaluated"]];

    headsOn  = freeHeads[onApplied];
    headsOff = freeHeads[offApplied];

    report[3, sc["tag"] <> ": RGEs on and off leave the same symbols free",
           headsOn === headsOff,
           "off: " <> ToString[headsOff] <> "\n          on : " <> ToString[headsOn]];

    (* dictPrinterUVcoup turns whatever Variables[] returns into run-card
       parameters, so it must contain nothing but declared UV couplings. *)
    declaredUV = parametersListFromMatchingResult[sc["file"], sc["loop"]][[2]];
    extra      = Complement[headsOn, declaredUV];

    report[3, sc["tag"] <> ": no non-UV symbol would reach the run card",
           extra === {},
           If[extra === {}, "free symbols: " <> ToString[headsOn],
              "would be printed as UV fit parameters: " <> ToString[extra]]];
  ],
  {i, Length[$scenarios]}];

(* ============================================================== SECTION 4 == *)

section["4. Matching scale vs. the endpoint the RGEs were integrated to"];

Module[{rep, ifs, domain, tMax, muMaxTeV, heavy, g3Heavy, g3AtMax},
  rep = replaceSMparamsMatchMakerEFT;
  ifs = interpFunctionsIn[rep];

  If[ifs === {},
    report[4, "RGE integration range is known", False, "no InterpolatingFunction found"];
    ,
    domain   = First[ifs][[1]];
    tMax     = Last[First[domain]];
    muMaxTeV = 10^tMax / 10^3;

    report[4, "RGE integration range is known", True,
           "the RGEs are integrated up to \[Mu] = " <> ToString[muMaxTeV] <> " TeV"];

    heavy   = 2 muMaxTeV;
    g3Heavy = Quiet@smCouplingAt["g3", heavy];
    g3AtMax = Quiet@smCouplingAt["g3", muMaxTeV];

    report[4, "a matching scale above the endpoint is rejected rather than extrapolated",
           !NumericQ[g3Heavy],
           "\[Mu] = " <> ToString[heavy] <> " TeV (above the " <> ToString[muMaxTeV] <>
           " TeV endpoint) gives g3 = " <> ToString[g3Heavy] <>
           ", vs g3 = " <> ToString[g3AtMax] <> " at the endpoint;\n          " <>
           "InterpolatingFunction extrapolates instead of failing, so a heavier "<>
           "model is matched with silently wrong SM couplings"];
  ];
];

(* ============================================================== SECTION 5 == *)

section["5. Do the SM RGE settings actually change the running?"];

Module[{g3One, g3Three, g3Int, g3LL},
  setSMRGELoop[1]; setSMRGEintegration["integrate"];
  g3One = smCouplingAt["g3", 3];

  setSMRGELoop[3]; setSMRGEintegration["integrate"];
  g3Three = smCouplingAt["g3", 3];

  report[5, "setSMRGELoop changes g3(3 TeV)",
         NumericQ[g3One] && NumericQ[g3Three] && relDiff[g3One, g3Three] > 10^-8,
         "g3(3 TeV): 1-loop = " <> ToString[g3One] <>
         " , 3-loop = " <> ToString[g3Three] <>
         If[NumericQ[g3One] && NumericQ[g3Three] && relDiff[g3One, g3Three] <= 10^-8,
            "\n          identical: the loop order never reaches the equations "<>
            "SMRunRGEs integrates", ""]];

  setSMRGELoop[$rgeLoopOrder]; setSMRGEintegration["integrate"];
  g3Int = smCouplingAt["g3", 3];
  setSMRGEintegration["leadinglog"];
  g3LL = smCouplingAt["g3", 3];

  report[5, "setSMRGEintegration changes g3(3 TeV)",
         NumericQ[g3Int] && NumericQ[g3LL] && relDiff[g3Int, g3LL] > 10^-8,
         "g3(3 TeV): integrate = " <> ToString[g3Int] <>
         " , leadinglog = " <> ToString[g3LL] <>
         If[NumericQ[g3Int] && NumericQ[g3LL] && relDiff[g3Int, g3LL] <= 10^-8,
            "\n          identical: the integration mode is never read, both "<>
            "branches run the full NDSolve", ""]];

  rgesOn[];
];

(* ============================================================== SECTION 6 == *)

section["6. Mass-scan cards (dictPrinterUVmass)"];

(* dictPrinterUVmass itself calls NotebookDirectory[] to pick where to write the
   card, which is $Failed outside the front end, so it cannot be called directly
   from a headless script (same constraint as dictPrinterUVcoup). This mirrors
   its internal logic exactly (match2fit.wl:1486-1501), in the same spirit as
   massHandler being tested directly in sections 2/3, so that this is a check
   of the real fix rather than of a reimplementation.

   Issue 1b (SM couplings evaluated at a fixed placeholder that has nothing to
   do with the scanned mass) is not something a full fix can remove with the
   current SMEFiT card format -- see tests/SM_RGE_open_issues.md. The agreed
   intermediate behaviour instead is:
     - by default ("MaxMass"->None) the SM RGE evolution is forced OFF for the
       mass-scan card only, regardless of the global setting;
     - if the caller opts in with a numeric MaxMass, the running (if switched
       on globally) is evaluated once at that fixed scale, and MaxMass also
       sets the printed mass upper bound.
   Both halves of that contract are asserted below. *)
massScanDict[file_, loop_, maxMass_: None] :=
  Module[{dicTotal, massString, massReemp, muOnly},
    {dicTotal, massString, massReemp} = If[maxMass === None,
      Block[{match2fit`Private`$loopLevelSMRGE = 0,
             match2fit`Private`$integLevelSMRGE = "None"},
        match2fit`Private`massHandler[file, 1, loop, {}]],
      match2fit`Private`massHandler[file, maxMass, loop, {}]
    ];
    muOnly   = Select[massReemp, SymbolName[#[[1]]] === "\[Mu]" &];
    dicTotal = (dicTotal /. muOnly) //.
                 hf_InterpolatingFunction[arg_] :> hf[N[arg]];
    dicTotal = dicTotal /. {Log[a_] :> 0};
    {dicTotal, massReemp}
  ];

$massScanScenarios = {
  <|"tag" -> "mass-scan, tree-level",
    "file" -> "sample_results/MatchingResult_Q1_U.dat", "loop" -> 0|>,
  <|"tag" -> "mass-scan, one-loop",
    "file" -> "sample_results/MatchingResult_S_oneloop.dat", "loop" -> 1|>
};

Do[
  Module[{sc = $massScanScenarios[[i]], loopBefore, integBefore,
          dicDefault, massReempDefault, dicMax, massReempMax, massSym,
          leftoverMu, leftoverIF, indet, valsDefault, valsMax},
    loopBefore  = statusSMRGE[];
    integBefore = integrationSMRGE[];

    {dicDefault, massReempDefault} = massScanDict[sc["file"], sc["loop"]];
    {dicMax, massReempMax}         = massScanDict[sc["file"], sc["loop"], 10];

    report[6, sc["tag"] <> ": default MaxMass->None leaves the global SM RGE "<>
              "setting untouched",
           statusSMRGE[] === loopBefore && integrationSMRGE[] === integBefore,
           "before: " <> ToString[{loopBefore, integBefore}] <>
           " ; after: " <> ToString[{statusSMRGE[], integrationSMRGE[]}]];

    Do[
      Module[{dicTotal = dc[[1]], massReemp = dc[[2]], tag = dc[[3]]},
        massSym    = SelectFirst[massReemp, SymbolName[#[[1]]] =!= "\[Mu]" &][[1]];
        leftoverMu = muSymbolsIn[dicTotal[[All, 2]]];
        leftoverIF = interpFunctionsIn[dicTotal[[All, 2]]];
        indet      = !FreeQ[dicTotal[[All, 2]], Indeterminate | ComplexInfinity];

        report[6, sc["tag"] <> " (" <> tag <> "): no residual \[Mu] in the dictionary",
               leftoverMu === {},
               If[leftoverMu === {}, "", "leftover: " <> ToString[leftoverMu]]];

        report[6, sc["tag"] <> " (" <> tag <> "): no unevaluated InterpolatingFunction",
               leftoverIF === {},
               If[leftoverIF === {}, "",
                  ToString[Length[leftoverIF]] <> " left unevaluated"]];

        report[6, sc["tag"] <> " (" <> tag <> "): no Indeterminate/ComplexInfinity "<>
                  "reaches the card",
               !indet,
               If[indet,
                  "the log-killing rule zeroed the log implicit in the running "<>
                  "coupling's own \[Mu]-argument, turning it into 0/0", ""]];

        report[6, sc["tag"] <> " (" <> tag <> "): the heavy mass is still a free symbol",
               MemberQ[Variables[dicTotal[[All, 2]]], massSym],
               "mass symbol: " <> ToString[massSym]];
      ],
      {dc, {{dicDefault, massReempDefault, "default"}, {dicMax, massReempMax, "MaxMass->10"}}}];

    valsDefault = dicDefault[[All, 2]];
    valsMax     = dicMax[[All, 2]];

    (* Vacuousness has to be checked on a RAW dictionary (RGE on, before
       massScanDict's own InterpolatingFunction->N[] evaluation, which would
       otherwise always report {} regardless of whether an SM coupling was
       ever present). Global RGE state here is "on" (rgesOn[] at the end of
       section 5), so this reuses that without re-toggling anything. *)
    If[Length[interpFunctionsIn[
         match2fit`Private`massHandler[sc["file"], 10, sc["loop"], {}][[1]][[All, 2]]]] === 0,
      info[sc["tag"] <> ": no SM coupling enters this matching result, so the "<>
           "default-vs-MaxMass comparison below is vacuous"];
      ,
      report[6, sc["tag"] <> ": MaxMass->10 (RGE on) numerically differs from the "<>
                "RGE-forced-off default",
             valsDefault =!= valsMax,
             "if these are identical either the RGE-off override in the default "<>
             "branch or the MaxMass override in the other branch is not taking "<>
             "effect"];
    ];
  ],
  {i, Length[$massScanScenarios]}];

(* ---------------------------------------------------------------- summary -- *)

Module[{nPass, nFail},
  nPass = Count[$results, {_, _, True}];
  nFail = Length[$results] - nPass;
  Print["\n================================================================"];
  Print["  ", nPass, " passed, ", nFail, " failed, ", Length[$results], " total"];
  If[nFail > 0,
    Print["\n  Failures:"];
    Do[Print["    (section ", r[[1]], ") ", r[[2]]], {r, Select[$results, !#[[3]] &]}];
  ];
  Print["================================================================"];
  Exit[If[nFail > 0, 1, 0]];
];
