(* ::Package:: *)

flavourSym=Fl2;
cutOffPart=48;(*Maximum: 48*)
wcVarDecomp[wc_] :=
    Block[{var, exp},
        var = Variables[wc];
        exp = Exponent[wc, var];
        SortBy[Table[{var[[i]], exp[[i]]}, {i, 1, Length[var]}], (#[[
            2]])&]];
requiredWCs = {{"WC", "Flavour indices"}, {Subscript[wwC, \[Phi]G], {}}, {Subscript[wwC, \[Phi]B], {}}, {Subscript[wwC, \[Phi]W], {}}, {Subscript[wwC, \[Phi]WB
    ], {}}, {Subscript[wwC, \[Phi]Sq], {}}, {Subscript[wwC, \[Phi]D], {}}, {Subscript[
    wwC, W], {}}, {Subscript[wwC, \[Phi]q1], {2, 2}}, {Subscript[wwC, \[Phi]q1], {3,
     3}}, {Subscript[wwC, \[Phi]q3], {2, 2}}, {Subscript[wwC, \[Phi]q3], {3, 3}}, {
    Subscript[wwC, \[Phi]L1], {1, 1}}, {Subscript[wwC, \[Phi]L1], {2, 2}}, {Subscript[
    wwC, \[Phi]L1], {3, 3}}, {Subscript[wwC, \[Phi]L3], {1, 1}}, {Subscript[wwC, \[Phi]L3
    ], {2, 2}}, {Subscript[wwC, \[Phi]L3], {3, 3}}, {Subscript[wwC, \[Phi]u], {2, 2
    }}, {Subscript[wwC, \[Phi]u], {3, 3}}, {Subscript[wwC, \[Phi]e], {1, 1}}, {Subscript[
    wwC, \[Phi]e], {2, 2}}, {Subscript[wwC, \[Phi]e], {3, 3}}, {Subscript[wwC, \[Phi]d],
     {3, 3}}, {Subscript[wwC, e\[Phi]], {3, 3}}, {Subscript[wwC, e\[Phi]], {2, 2}},
     {Subscript[wwC, e\[Phi]], {1, 1}}, {Subscript[wwC, u\[Phi]], {3, 3}}, {Subscript[
    wwC, d\[Phi]], {3, 3}}, {Subscript[wwC, u\[Phi]], {2, 2}}, {Subscript[wwC, uB],
     {3, 3}}, {Subscript[wwC, uW], {3, 3}}, {Subscript[wwC, ll], {1, 2, 2,
     1}}, {Subscript[wwC, ll], {1, 1, 1,
     1}}, {Subscript[wwC, qq1], {1, 3, 3, 1}}, {Subscript[wwC, qq1], {3, 
    3, 3, 3}}, {Subscript[wwC, qq3], {1, 3, 3, 1}}, {Subscript[wwC, qq3],
     {1, 1, 3, 3}}, {Subscript[wwC, qq3], {3, 3, 3, 3}}, {Subscript[wwC, 
    qu1], {3, 3, 1, 1}}, {Subscript[wwC, qu1], {1, 1, 3, 3}}, {Subscript[
    wwC, qu1], {3, 3, 3, 3}}, {Subscript[wwC, qu8], {3, 3, 1, 1}}, {Subscript[
    wwC, qu8], {1, 1, 3, 3}}, {Subscript[wwC, qu8], {3, 3, 3, 3}}, {Subscript[
    wwC, uu], {1, 1, 3, 3}}, {Subscript[wwC, uu], {1, 3, 3, 1}}, {Subscript[
    wwC, uu], {3, 3, 3, 3}}, {Subscript[wwC, qq1], {1, 3, 3, 1}}, {Subscript[
    wwC, qq3], {1, 3, 3, 1}}, {Subscript[wwC, qq3], {1, 1, 3, 3}}, {Subscript[
    wwC, ud8], {3, 3, 1, 1}}, {Subscript[wwC, ud1], {3, 3, 1, 1}}, {Subscript[
    wwC, qd8], {3, 3, 1, 1}}, {Subscript[wwC, qd1], {3, 3, 1, 1}},{Subscript[wwC, \[Phi]], {}}};
    (*/////////*)
    requiredWCsmeftsimBasis = {{"WC SMEFTSim", "WC Warsaw"}, {wwC\[CurlyPhi]G, Hold[
    Subscript[wC, \[Phi]G]]}, {wwC\[CurlyPhi]B, Hold[Subscript[wC, \[Phi]B]]}, {wwC\[CurlyPhi]W, Hold[Subscript[
    wC, \[Phi]W]]}, {wwC\[CurlyPhi]WB, Hold[Subscript[wC, \[Phi]WB]]}, {wwC\[CurlyPhi]d, Hold[-Subscript[
    wC, \[Phi]Sq]]}, {wwC\[CurlyPhi]D, Hold[Subscript[wC, \[Phi]D]]}, {wwCWWW, Hold[Subscript[
    wC, W]]}, {wwC\[CurlyPhi]qm, Hold[Subscript[wC, \[Phi]q1][2, 2] - Subscript[wC, \[Phi]q3][
    2, 2]]}, {wwC\[CurlyPhi]Qm, Hold[Subscript[wC, \[Phi]q1][3, 3] - Subscript[wC, \[Phi]q3][
    3, 3]]}, {wwC\[CurlyPhi]q3, Hold[Subscript[wC, \[Phi]q3][2, 2]]}, {wwC\[CurlyPhi]Q3, Hold[Subscript[
    wC, \[Phi]q3][3, 3]]}, {wwC\[CurlyPhi]l11, Hold[Subscript[wC, \[Phi]L1][1, 1]]}, {wwC\[CurlyPhi]l12,
     Hold[Subscript[wC, \[Phi]L1][2, 2]]}, {wwC\[CurlyPhi]l13, Hold[Subscript[wC, \[Phi]L1][3,
     3]]}, {wwC\[CurlyPhi]l31, Hold[Subscript[wC, \[Phi]L3][1, 1]]}, {wwC\[CurlyPhi]l32, Hold[Subscript[
    wC, \[Phi]L3][2, 2]]}, {wwC\[CurlyPhi]l33, Hold[Subscript[wC, \[Phi]L3][3, 3]]}, {wwC\[CurlyPhi]ui,
     Hold[Subscript[wC, \[Phi]u][2, 2]]}, {wwC\[CurlyPhi]di, Hold[Subscript[wC, \[Phi]d][2, 2
    ]]}, {wwC\[CurlyPhi]t, Hold[Subscript[wC, \[Phi]u][3, 3]]}, {wwC\[CurlyPhi]e, Hold[Subscript[wC,
     \[Phi]e][1, 1]]}, {wwC\[CurlyPhi]\[Mu], Hold[Subscript[wC, \[Phi]e][2, 2]]}, {wwC\[CurlyPhi]\[Tau], Hold[Subscript[
    wC, \[Phi]e][3, 3]]}, {wwCQq18, Hold[Subscript[wC, qq1][1, 3, 3, 1] + 3 Subscript[
    wC, qq3][1, 3, 3, 1]]}, {wwCQq11, Hold[Subscript[wC, qq1][1, 1, 3, 3]
     + (1 / 6) Subscript[wC, qq1][1, 3, 3, 1]] + (1 / 2) Subscript[wC, qq3
    ][1, 3, 3, 1]}, {wwCQq38, Hold[Subscript[wC, qq1][1, 3, 3, 1] - Subscript[
    wC, qq3][1, 3, 3, 1]]}, {wwCQq31, Hold[Subscript[wC, qq3][1, 1, 3, 3]
     + (1 / 6) Subscript[wC, qq1][1, 3, 3, 1] - (1 / 6) Subscript[wC, qq3
    ][1, 3, 3, 1]]}, {wwCtq8, Hold[Subscript[wC, qu8][1, 1, 3, 3]]}, {wwCtq1,
     Hold[Subscript[wC, qu1][1, 1, 3, 3]]}, {wwCtu8, Hold[2 * Subscript[wC,
     uu][1, 3, 3, 1]]}, {wwCtu1, Hold[Subscript[wC, uu][1, 1, 3, 3] + (1 
    / 3) Subscript[wC, uu][1, 3, 3, 1]]}, {wwCQu8, Hold[Subscript[wC, qu8
    ][3, 3, 1, 1]]}, {wwCQu1, Hold[Subscript[wC, qu1][3, 3, 1, 1]]}, {wwCtd8,
     Hold[Subscript[wC, ud8][3, 3, 3, 3]]}, {wwCtd1, Hold[Subscript[wC, ud1
    ][3, 3, 3, 3]]}, {wwCQd8, Hold[Subscript[wC, qd8][3, 3, 3, 3]]}, {wwCQd1,
     Hold[Subscript[wC, qd1][3, 3, 3, 3]]}, {wwCQQ1, Hold[2 Subscript[wC,
     qq1][3, 3, 3, 3] - (2 / 3) Subscript[wC, qq3][3, 3, 3, 3]]}, {wwCQQ8,
     Hold[8 Subscript[wC, qq3][3, 3, 3, 3]]}, {wwCQt1, Hold[Subscript[wC,
     qu1][3, 3, 3, 3]]}, {wwCQt8, Hold[Subscript[wC, qu8][3, 3, 3, 3]]}, 
    {wwCtt1, Hold[Subscript[wC, uu][3, 3, 3, 3]]}, {wwCll, Hold[Subscript[
    wC, ll][1, 2, 2, 1]]},{wwCll1111, Hold[Subscript[
    wC, ll][1, 1, 1, 1]]}, {wwCb\[CurlyPhi], Hold[Subscript[wC, d\[Phi]][3, 3]]}, {wwCt\[CurlyPhi],
     Hold[Subscript[wC, u\[Phi]][3, 3]]}, {wwCtG, Hold[Subscript[wC, uG][3, 3]
    ]}, {wwCc\[CurlyPhi], Hold[Subscript[wC, u\[Phi]][2, 2]]}, {wwC\[Tau]\[CurlyPhi], Hold[Subscript[wC,
     e\[Phi]][3, 3]]}, {wwCtW, Hold[Subscript[wC, uW][3, 3]]},{wwCtZ, Hold[-sW
     * Subscript[wC, uB][3, 3] + cW * Subscript[wC, uB][3, 3]]},{wwC\[CurlyPhi], Hold[
    Subscript[wC, \[Phi]]]}};
     (*/////////*)
     convToSMEFiTbasis = {Subscript[wwC, \[Phi]Sq] -> -wwC\[CurlyPhi]d, Subscript[wwC, qq3
    ][3, 3, 3, 3] -> (1 / 8) wwCQQ8, Subscript[wwC, qq1][1, 1, 1, 1] -> (
    1 / 2) wwCQQ1 + (1 / 24) wwCQQ8, Subscript[wwC, qq1][1, 3, 3, 1] -> (
    1 / 4) wwCQq18 + (3 / 4) wwCQq38, Subscript[wwC, qq3][1, 3, 3, 1] -> 
    (1 / 4) (wwCQq18 - wwCQq38), Subscript[wwC, qq1][1, 1, 3, 3] -> wwCQq11
     - (1 / 6) wwCQq18, Subscript[wwC, qq3][1, 1, 3, 3] -> wwCQq31 - (1 /
     6) wwCQq38, wwCuu[1, 3, 3, 1] -> (1 / 2) wwCtu8, wwCuu[1, 1, 3, 3] ->
     wwCtu1 - (1 / 6) wwCtu8};
     printNameWCs[x_] :=
    Piecewise[{{"c81qq", ToString[x] == "wwCQq18"}, {"c11qq", ToString[
        x] == "wwCQq11"}, {"c83qq", ToString[x] == "wwCQq38"}, {"c13qq", ToString[
        x] == "wwCQq31"}, {"c8qt", ToString[x] == "wwCtq8"}, {"c1qt", ToString[
        x] == "wwCtq1"}, {"c8ut", ToString[x] == "wwCtu8"}, {"c1ut", ToString[
        x] == "wwCtu1"}, {"c8qu", ToString[x] == "wwCQu8"}, {"c1qu", ToString[
        x] == "wwCQu1"}, {"c8dt", ToString[x] == "wwCtd8"}, {"c1dt", ToString[
        x] == "wwCtd1"}, {"c8qd", ToString[x] == "wwCQd8"}, {"c1qd", ToString[
        x] == "wwCQd1"}, {"cQQ1", ToString[x] == "wwCQQ1"}, {"cQQ8", ToString[
        x] == "wwCQQ8"}, {"cQt1", ToString[x] == "wwCQt1"}, {"cQt8", ToString[
        x] == "wwCQt8"}, {"ctt1", ToString[x] == "wwCtt1"}, {"cll", ToString[
        x] == "wwCll"},{"cll1111",ToString[x]=="wwCll1111"}, {"cbp", ToString[x] == "wwCb\[CurlyPhi]"}, {"ctp", ToString[x] 
        == "wwCt\[CurlyPhi]"}, {"ctG", ToString[x] == "wwCtG"}, {"ccp", ToString[x] == 
        "wwCc\[CurlyPhi]"}, {"ctap", ToString[x] == "wwC\[Tau]\[CurlyPhi]"}, {"ctW", ToString[x] == "wwCtW"
        }, {"ctZ", ToString[x] == "wwCtZ"}, {"c3pQ3", ToString[x] == "wwC\[CurlyPhi]Q3"
        }, {"cpQM", ToString[x] == "wwC\[CurlyPhi]Qm"}, {"cpt", ToString[x] == "wwC\[CurlyPhi]t"},
         {"cpl1", ToString[x] == "wwC\[CurlyPhi]l11"}, {"c3pl1", ToString[x] == "wwC\[CurlyPhi]l31"
        }, {"cpl2", ToString[x] == "wwC\[CurlyPhi]l12"}, {"c3pl2", ToString[x] == "wwC\[CurlyPhi]l32"
        }, {"cpl3", ToString[x] == "wwC\[CurlyPhi]l13"}, {"c3pl3", ToString[x] == "wwC\[CurlyPhi]l33"
        }, {"cpe", ToString[x] == "wwC\[CurlyPhi]e"}, {"cpmu", ToString[x] == "wwC\[CurlyPhi]\[Mu]"},
         {"cpta", ToString[x] == "wwC\[CurlyPhi]\[Tau]"}, {"c3pq", ToString[x] == "wwC\[CurlyPhi]q3"},
         {"cpqMi", ToString[x] == "wwC\[CurlyPhi]qm"}, {"cpui", ToString[x] == "wwC\[CurlyPhi]ui"
        }, {"cpdi", ToString[x] == "wwC\[CurlyPhi]di"}, {"cpG", ToString[x] == "wwC\[CurlyPhi]G"},
         {"cpB", ToString[x] == "wwC\[CurlyPhi]B"}, {"cpW", ToString[x] == "wwC\[CurlyPhi]W"}, {"cpd",
         ToString[x] == "wwC\[CurlyPhi]d"}, {"cpWB", ToString[x] == "wwC\[CurlyPhi]WB"}, {"cpD", 
        ToString[x] == "wwC\[CurlyPhi]D"}, {"cWWW", ToString[x] == "wwCWWW"},{"cp",ToString[x]=="wwC\[CurlyPhi]"}}];
        (*///////////////////*)
        readNameWCs[x_] :=
    Piecewise[{{"wwCQq18", x == "O81qq"}, {"wwCQq11", x == "O11qq"}, 
        {"wwCQq38", x == "O83qq"}, {"wwCQq31", x == "O13qq"}, {"wwCtq8", x ==
         "O8qt"}, {"wwCtq1", x == "O1qt"}, {"wwCtu8", x == "O8ut"}, {"wwCtu1",
         x == "O1ut"}, {"wwCQu8", x == "O8qu"}, {"wwCQu1", x == "O1qu"}, {"wwCtd8",
         x == "O8dt"}, {"wwCtd1", x == "O1dt"}, {"wwCQd8", x == "O8qd"}, {"wwCQd1",
         x == "O1qd"}, {"wwCQQ1", x == "OQQ1"}, {"wwCQQ8", x == "OQQ8"}, {"wwCQt1",
         x == "OQt1"}, {"wwCQt8", x == "OQt8"}, {"wwCtt1", x == "Ott1"}, {"wwCll",
         x == "Oll"},{"wwCll1111",
         x == "Oll1111"}, {"wwCb\[CurlyPhi]", x == "Obp"}, {"wwCt\[CurlyPhi]", x == "Otp"}, {"wwCtG",
         x == "OtG"}, {"wwCc\[CurlyPhi]", x == "Ocp"}, {"wwC\[Tau]\[CurlyPhi]", x == "Otap"}, {"wwCtW",
         x == "OtW"}, {"wwCtZ", x == "OtZ"}, {"wwC\[CurlyPhi]Q3", x == "O3pQ3"}, {"wwC\[CurlyPhi]Qm",
         x == "OpQM"}, {"wwC\[CurlyPhi]t", x == "Opt"}, {"wwC\[CurlyPhi]l11", x == "Opl1"}, {"wwC\[CurlyPhi]l31",
         x == "O3pl1"}, {"wwC\[CurlyPhi]l12", x == "Opl2"}, {"wwC\[CurlyPhi]l32", x == "O3pl2"}, 
        {"wwC\[CurlyPhi]l13", x == "Opl3"}, {"wwC\[CurlyPhi]l33", x == "O3pl3"}, {"wwC\[CurlyPhi]e", x == "Ope"
        }, {"wwC\[CurlyPhi]\[Mu]", x == "Opmu"}, {"wwC\[CurlyPhi]\[Tau]", x == "Opta"}, {"wwC\[CurlyPhi]q3", x == "O3pq"
        }, {"wwC\[CurlyPhi]qm", x == "OpqMi"}, {"wwC\[CurlyPhi]ui", x == "Opui"}, {"wwC\[CurlyPhi]di", x ==
         "Opdi"}, {"wwC\[CurlyPhi]G", x == "OpG"}, {"wwC\[CurlyPhi]B", x == "OpB"}, {"wwC\[CurlyPhi]W", x ==
         "OpW"}, {"wwC\[CurlyPhi]d", x == "Opd"}, {"wwC\[CurlyPhi]WB", x == "OpWB"}, {"wwC\[CurlyPhi]D", x 
        == "OpD"}, {"wwCWWW", x == "OWWW"},{"wwC\[CurlyPhi]",x=="Op"}}];
        (*///////////////////*)
        readNameWCsInv[x_] :=
    Evaluate[Piecewise[{Permute[{x == "wwCQq18", "O81qq"}, {2, 1}], Permute[
        {x == "wwCQq11", "O11qq"}, {2, 1}], Permute[{x == "wwCQq38", "O83qq"},
         {2, 1}], Permute[{x == "wwCQq31", "O13qq"}, {2, 1}], Permute[{x == "wwCtq8",
         "O8qt"}, {2, 1}], Permute[{x == "wwCtq1", "O1qt"}, {2, 1}], Permute[
        {x == "wwCtu8", "O8ut"}, {2, 1}], Permute[{x == "wwCtu1", "O1ut"}, {2,
         1}], Permute[{x == "wwCQu8", "O8qu"}, {2, 1}], Permute[{x == "wwCQu1",
         "O1qu"}, {2, 1}], Permute[{x == "wwCtd8", "O8dt"}, {2, 1}], Permute[
        {x == "wwCtd1", "O1dt"}, {2, 1}], Permute[{x == "wwCQd8", "O8qd"}, {2,
         1}], Permute[{x == "wwCQd1", "O1qd"}, {2, 1}], Permute[{x == "wwCQQ1",
         "OQQ1"}, {2, 1}], Permute[{x == "wwCQQ8", "OQQ8"}, {2, 1}], Permute[
        {x == "wwCQt1", "OQt1"}, {2, 1}], Permute[{x == "wwCQt8", "OQt8"}, {2,
         1}], Permute[{x == "wwCtt1", "Ott1"}, {2, 1}], Permute[{x == "wwCll",
         "Oll"}, {2, 1}],Permute[{x == "wwCll1111",
         "Oll1111"}, {2, 1}], Permute[{x == "wwCb\[CurlyPhi]", "Obp"}, {2, 1}], Permute[{x 
        == "wwCt\[CurlyPhi]", "Otp"}, {2, 1}], Permute[{x == "wwCtG", "OtG"}, {2, 1}], 
        Permute[{x == "wwCc\[CurlyPhi]", "Ocp"}, {2, 1}], Permute[{x == "wwC\[Tau]\[CurlyPhi]", "Otap"
        }, {2, 1}], Permute[{x == "wwCtW", "OtW"}, {2, 1}], Permute[{x == "wwCtZ",
         "OtZ"}, {2, 1}], Permute[{x == "wwC\[CurlyPhi]Q3", "O3pQ3"}, {2, 1}], Permute[
        {x == "wwC\[CurlyPhi]Qm", "OpQM"}, {2, 1}], Permute[{x == "wwC\[CurlyPhi]t", "Opt"}, {2, 
        1}], Permute[{x == "wwC\[CurlyPhi]l11", "Opl1"}, {2, 1}], Permute[{x == "wwC\[CurlyPhi]l31",
         "O3pl1"}, {2, 1}], Permute[{x == "wwC\[CurlyPhi]l12", "Opl2"}, {2, 1}], Permute[
        {x == "wwC\[CurlyPhi]l32", "O3pl2"}, {2, 1}], Permute[{x == "wwC\[CurlyPhi]l13", "Opl3"},
         {2, 1}], Permute[{x == "wwC\[CurlyPhi]l33", "O3pl3"}, {2, 1}], Permute[{x == "wwC\[CurlyPhi]e",
         "Ope"}, {2, 1}], Permute[{x == "wwC\[CurlyPhi]\[Mu]", "Opmu"}, {2, 1}], Permute[{x
         == "wwC\[CurlyPhi]\[Tau]", "Opta"}, {2, 1}], Permute[{x == "wwC\[CurlyPhi]q3", "O3pq"}, {2, 1
        }], Permute[{x == "wwC\[CurlyPhi]qm", "OpqMi"}, {2, 1}], Permute[{x == "wwC\[CurlyPhi]ui",
         "Opui"}, {2, 1}], Permute[{x == "wwC\[CurlyPhi]di", "Opdi"}, {2, 1}], Permute[
        {x == "wwC\[CurlyPhi]G", "OpG"}, {2, 1}], Permute[{x == "wwC\[CurlyPhi]B", "OpB"}, {2, 1}
        ], Permute[{x == "wwC\[CurlyPhi]W", "OpW"}, {2, 1}], Permute[{x == "wwC\[CurlyPhi]d", "Opd"
        }, {2, 1}], Permute[{x == "wwC\[CurlyPhi]WB", "OpWB"}, {2, 1}], Permute[{x == "wwC\[CurlyPhi]D",
         "OpD"}, {2, 1}], Permute[{x == "wwCWWW", "OWWW"}, {2, 1}],Permute[{x=="wwC\[CurlyPhi]","Op"}, {2, 1}]}]];
mmEFTout={{alphaKB->1,alphaKG->1,alphaKH->1,alphaKW->1,alphaKB->1,
alphaO3G->Hold[Subscript[wC, G]],
alphaO3Gt->Hold[Subscript[wC, Gtil]],
alphaO3W->0,alphaO3Wt->0,
alphaOH->Hold[Subscript[wC, \[Phi]]],
alphaOHB->Hold[Subscript[wC, \[Phi]B]],
alphaOHBox->Hold[Subscript[wC, \[Phi]Sq]],
alphaOHBt->Hold[Subscript[wC, \[Phi]Btil]],
alphaOHD->Hold[Subscript[wC, \[Phi]D]],
alphaOHG->0,
alphaOHGt->0,
alphaOHW->Hold[Subscript[wC, \[Phi]W]],
alphaOHWB->Hold[Subscript[wC, \[Phi]WB]],
alphaOHWBt->Hold[Subscript[wC, \[Phi]WBtil]],
alphaOHWt->Hold[Subscript[wC, \[Phi]Wtil]],
alphaOlambda->Hold[\[Lambda]Hat[\[Phi]]],alphaOmuH2->Hold[\[Mu]Hat[\[Phi]]^2],
alphaKd[mif1_,mif2_]->KroneckerDelta[mif1,mif2],alphaKdbar[mif1_,mif2_]->KroneckerDelta[mif1,mif2],
alphaKe[mif1_,mif2_]->KroneckerDelta[mif1,mif2],alphaKebar[mif1_,mif2_]->KroneckerDelta[mif1,mif2],
alphaKl[mif1_,mif2_]->KroneckerDelta[mif1,mif2],alphaKlbar[mif1_,mif2_]->KroneckerDelta[mif1,mif2],
alphaKq[mif1_,mif2_]->KroneckerDelta[mif1,mif2],alphaKqbar[mif1_,mif2_]->KroneckerDelta[mif1,mif2],
alphaKu[mif1_,mif2_]->KroneckerDelta[mif1,mif2],alphaKubar[mif1_,mif2_]->KroneckerDelta[mif1,mif2],
alphaOddbar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, dd][mif1,mif2,mif3,mif4]]],
alphaOeebar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, ee][mif1,mif2,mif3,mif4]]],
alphaOHdbar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, \[Phi]d][mif1,mif2]]],
alphaOHebar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, \[Phi]e][mif1,mif2]]],
alphaOHl1bar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, \[Phi]L1][mif1,mif2]]],
alphaOHl3bar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, \[Phi]L3][mif1,mif2]]],
alphaOHq1bar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, \[Phi]q1][mif1,mif2]]],
alphaOHq3bar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, \[Phi]q3][mif1,mif2]]],
alphaOHubar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, \[Phi]u][mif1,mif2]]],
alphaOllbar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, ll][mif1,mif2,mif3,mif4]]],
alphaOuubar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, uu][mif1,mif2,mif3,mif4]]],
alphaWeinberg[mif1_,mif2_]->0,alphaWeinbergbar[mif1_,mif2_]->0,
alphaOdB[mif1_,mif2_]->Hold[Subscript[wC, dB][mif1,mif2]],
alphaOdBbar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, dB][mif1,mif2]]],
alphaOdd[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, dd][mif1,mif2,mif3,mif4]],
alphaOdG[mif1_,mif2_]->Hold[Subscript[wC, dG][mif1,mif2]],
alphaOdGbar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, dG][mif1,mif2]]],
alphaOdH[mif1_,mif2_]->Hold[Subscript[wC, d\[Phi]][mif1,mif2]],
alphaOdHbar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, d\[Phi]][mif1,mif2]]],
alphaOdW[mif1_,mif2_]->Hold[Subscript[wC, dW][mif1,mif2]],
alphaOdWbar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, dW][mif1,mif2]]],
alphaOeB[mif1_,mif2_]->Hold[Subscript[wC, eB][mif1,mif2]],
alphaOeBbar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, eB][mif1,mif2]]],
alphaOed[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, ed][mif1,mif2,mif3,mif4]],
alphaOedbar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, ed][mif1,mif2,mif3,mif4]]],
alphaOee[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, ee][mif1,mif2,mif3,mif4]],
alphaOeH[mif1_,mif2_]->Hold[Subscript[wC, e\[Phi]][mif1,mif2]],
alphaOeHbar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, e\[Phi] ][mif1,mif2]]],
alphaOeu[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, eu][mif1,mif2,mif3,mif4]],
alphaOeubar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, eu][mif1,mif2,mif3,mif4]]],
alphaOeW[mif1_,mif2_]->Hold[Subscript[wC, eW][mif1,mif2]],
alphaOeWbar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, eW][mif1,mif2]]],
alphaOHd[mif1_,mif2_]->Hold[Subscript[wC, \[Phi]d][mif1,mif2]],
alphaOHe[mif1_,mif2_]->Hold[Subscript[wC, \[Phi]e][mif1,mif2]],
alphaOHl1[mif1_,mif2_]->Hold[Subscript[wC, \[Phi]L1][mif1,mif2]],
alphaOHl3[mif1_,mif2_]->Hold[Subscript[wC, \[Phi]L3][mif1,mif2]],
alphaOHq1[mif1_,mif2_]->Hold[Subscript[wC, \[Phi]q1][mif1,mif2]],
alphaOHq3[mif1_,mif2_]->Hold[Subscript[wC, \[Phi]q3][mif1,mif2]],
alphaOHu[mif1_,mif2_]->Hold[Subscript[wC, \[Phi]u][mif1,mif2]],
alphaOHud[mif1_,mif2_]->Hold[Subscript[wC, \[Phi]ud][mif1,mif2]],
alphaOHudbar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, \[Phi]ud][mif1,mif2]]],
alphaOld[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, ld][mif1,mif2,mif3,mif4]],
alphaOldbar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, ld][mif1,mif2,mif3,mif4]]],
alphaOle[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, le][mif1,mif2,mif3,mif4]],
alphaOlebar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, ld][mif1,mif2,mif3,mif4]]],
alphaOledq[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, ledq][mif1,mif2,mif3,mif4]],
alphaOledqbar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, ledq][mif1,mif2,mif3,mif4]]],
alphaOlequ1[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, lequ1][mif1,mif2,mif3,mif4]],
alphaOlequ1bar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, lequ1][mif1,mif2,mif3,mif4]]],
alphaOlequ3[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, lequ3][mif1,mif2,mif3,mif4]],
alphaOlequ3bar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, lequ3][mif1,mif2,mif3,mif4]]],
alphaOll[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, ll][mif1,mif2,mif3,mif4]],
alphaOlq1[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, lq1][mif1,mif2,mif3,mif4]],
alphaOlq1bar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, lq1][mif1,mif2,mif3,mif4]]],
alphaOlq3[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, lq3][mif1,mif2,mif3,mif4]],
alphaOlq3bar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, lq3][mif1,mif2,mif3,mif4]]],
alphaOlu[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, lu][mif1,mif2,mif3,mif4]],
alphaOlubar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, lu][mif1,mif2,mif3,mif4]]],
alphaOqd1[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, qd1][mif1,mif2,mif3,mif4]],
alphaOqd1bar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, qd1][mif1,mif2,mif3,mif4]]],
alphaOqd8[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, qd8][mif1,mif2,mif3,mif4]],
alphaOqd8bar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, qd8][mif1,mif2,mif3,mif4]]],
alphaOqe[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, qe][mif1,mif2,mif3,mif4]],
alphaOqebar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, qe][mif1,mif2,mif3,mif4]]],
alphaOqq1[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, qq1][mif1,mif2,mif3,mif4]],
alphaOqq1bar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, qq1][mif1,mif2,mif3,mif4]]],
alphaOqq3[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, qq3][mif1,mif2,mif3,mif4]],
alphaOqq3bar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, qq3][mif1,mif2,mif3,mif4]]],
alphaOqu1[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, qu1][mif1,mif2,mif3,mif4]],
alphaOqu1bar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, qu1][mif1,mif2,mif3,mif4]]],
alphaOqu8[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, qu8][mif1,mif2,mif3,mif4]],
alphaOqu8bar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, qu8][mif1,mif2,mif3,mif4]]],
alphaOquqd1[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, quqd1][mif1,mif2,mif3,mif4]],
alphaOquqd1bar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, quqd1][mif1,mif2,mif3,mif4]]],
alphaOquqd8[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, quqd8][mif1,mif2,mif3,mif4]],
alphaOquqd8bar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, quqd8][mif1,mif2,mif3,mif4]]],
alphaOuB[mif1_,mif2_]->Hold[Subscript[wC, uB][mif1,mif2]],
alphaOuBbar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, uB][mif1,mif2]]],
alphaOud1[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, ud1][mif1,mif2,mif3,mif4]],
alphaOud1bar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, ud1][mif1,mif2,mif3,mif4]]],
alphaOud8[mif1_,mif2_,mif3_,mif4_]->Hold[Subscript[wC, ud8][mif1,mif2,mif3,mif4]],
alphaOud8bar[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, ud8][mif1,mif2,mif3,mif4]]],
alphaOuG[mif1_,mif2_]->Hold[Subscript[wC, uG][mif1,mif2]],
alphaOuGbar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, uG][mif1,mif2]]],
alphaOuH[mif1_,mif2_]->Hold[Subscript[wC, u\[Phi]][mif1,mif2]],
alphaOuHbar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, u\[Phi]][mif1,mif2]]],
alphaOuu[mif1_,mif2_,mif3_,mif4_]->Hold[Conjugate[Subscript[wC, uu][mif1,mif2,mif3,mif4]]],
alphaOuW[mif1_,mif2_]->Hold[Subscript[wC, uW][mif1,mif2]],
alphaOuWbar[mif1_,mif2_]->Hold[Conjugate[Subscript[wC, uW][mif1,mif2]]],
alphaOlambdad[mif1_,mif2_]->yd[mif1,mif2],
alphaOlambdadbar[mif1_,mif2_]->Conjugate[yd[mif1,mif2]],
alphaOlambdae[mif1_,mif2_]->yl[mif1,mif2],
alphaOlambdaebar[mif1_,mif2_]->Conjugate[yl[mif1,mif2]],
alphaOlambdau[mif1_,mif2_]->yu[mif1,mif2],
alphaOlambdaubar[mif1_,mif2_]->Conjugate[yu[mif1,mif2]]
}
};


massAllocator[f_]:=Block[{aux,miNum},miNum=Min[n[f],Length[listM[f]]];If[miNum>0,For[i=1,i<=miNum,i++,M[f][i]=listM[f][[i]]];If[n[f]>miNum,For[i=miNum+1,i<=n[f],i++,M[f][i]=listM[f][[-1]];];];];];
setAllToZero[Scalars]:=For[i=1,i<=Length[nameListScalars],i++,n[nameListScalars[[i]]]=0;listM[nameListScalars[[i]]]={0.0};];
setAllToZero[Fermions]:=For[i=1,i<=Length[nameListFermions],i++,n[nameListFermions[[i]]]=0;listM[nameListFermions[[i]]]={0.0};];
setAllToZero[Vector]:=For[i=1,i<=Length[nameListVectors],i++,n[nameListVectors[[i]]]=0;listM[nameListVectors[[i]]]={0.0};];
setAllToZero[all]:=For[i=1,i<=Length[nameListAll],i++,n[nameListAll[[i]]]=0;listM[nameListAll[[i]]]={0.0};];
SetAttributes[n,Listable];
nameListScalars={S,S1,S2,\[CurlyPhi],\[CapitalXi],\[CapitalXi]1,\[CapitalTheta]1,\[CapitalTheta]3,\[Omega]1,\[Omega]2,\[Omega]4,\[CapitalPi]1,\[CapitalPi]7,\[Zeta],\[CapitalOmega]1,\[CapitalOmega]2,\[CapitalOmega]4,\[CapitalUpsilon],\[CapitalPhi]};
nameListVectors={B,B1,W,W1,G,G1,H,L1,L3,\[ScriptCapitalU]2,\[ScriptCapitalU]5,\[ScriptCapitalQ]1,\[ScriptCapitalQ]5,X,Y1,Y5};
nameListFermions={Ne,Ef,\[CapitalDelta]1,\[CapitalDelta]3,\[CapitalSigma],\[CapitalSigma]1,U,Df,Q1,Q5,Q7,T1,T2};
nameListAll=Join[nameListScalars,nameListVectors,nameListFermions];
nameListOperators[f1_,f2_,f3_,f4_]:={Subscript[wC, 5][f1,f2],Subscript[wC, ll][f1,f2,f3,f4],Subscript[wC, qq1][f1,f2,f3,f4],Subscript[wC, qq3][f1,f2,f3,f4],Subscript[wC, lq1][f1,f2,f3,f4],Subscript[wC, lq3][f1,f2,f3,f4],Subscript[wC, ee][f1,f2,f3,f4],Subscript[wC, dd][f1,f2,f3,f4],Subscript[wC, uu][f1,f2,f3,f4],Subscript[wC, ed][f1,f2,f3,f4],Subscript[wC, eu][f1,f2,f3,f4],Subscript[wC, ud1][f1,f2,f3,f4],Subscript[wC, ud8][f1,f2,f3,f4],Subscript[wC, le][f1,f2,f3,f4],Subscript[wC, ld][f1,f2,f3,f4],Subscript[wC, lu][f1,f2,f3,f4],Subscript[wC, qe][f1,f2,f3,f4],Subscript[wC, qu1][f1,f2,f3,f4],Subscript[wC, qu8][f1,f2,f3,f4],Subscript[wC, qd1][f1,f2,f3,f4],Subscript[wC, qd8][f1,f2,f3,f4],Subscript[wC, ledq][f1,f2,f3,f4],
Subscript[wC, quqd1][f1,f2,f3,f4],Subscript[wC, quqd8][f1,f2,f3,f4],Subscript[wC, lequ1][f1,f2,f3,f4],Subscript[wC, lequ3][f1,f2,f3,f4],Subscript[wC, duq][f1,f2,f3,f4],Subscript[wC, qqu][f1,f2,f3,f4],Subscript[wC, qqq][f1,f2,f3,f4],Subscript[wC, duu][f1,f2,f3,f4],Subscript[wC, W],Subscript[wC, Wtil],Subscript[wC, G],Subscript[wC, Gtil],Subscript[wC, \[Phi]],Subscript[wC, \[Phi]D],Subscript[wC, \[Phi]Sq],Subscript[wC, \[Phi]B],Subscript[wC, \[Phi]Btil],Subscript[wC, \[Phi]W],Subscript[wC, \[Phi]Wtil],Subscript[wC, \[Phi]WB],Subscript[wC, \[Phi]WBtil],Subscript[wC, \[Phi]G],Subscript[wC, \[Phi]Gtil],Subscript[wC, e\[Phi]][f1,f2],Subscript[wC, d\[Phi]][f1,f2],Subscript[wC, u\[Phi]][f1,f2],Subscript[wC, eB][f1,f2],Subscript[wC, eW][f1,f2],Subscript[wC, dB][f1,f2],Subscript[wC, uB][f1,f2],Subscript[wC, uW][f1,f2],Subscript[wC, dW][f1,f2],Subscript[wC, dG][f1,f2],Subscript[wC, uG][f1,f2],Subscript[wC, \[Phi]L1][f1,f2],Subscript[wC, \[Phi]L3][f1,f2],Subscript[wC, \[Phi]q1][f1,f2],Subscript[wC, \[Phi]q3][f1,f2],Subscript[wC, \[Phi]e][f1,f2],Subscript[wC, \[Phi]d][f1,f2],Subscript[wC, \[Phi]u][f1,f2],Subscript[wC, \[Phi]ud][f1,f2]};
nameListOperatorsNoEval[f1_,f2_,f3_,f4_]:={Subscript[wwC, 5][f1,f2],Subscript[wwC, ll][f1,f2,f3,f4],Subscript[wwC, qq1][f1,f2,f3,f4],Subscript[wwC, qq3][f1,f2,f3,f4],Subscript[wwC, lq1][f1,f2,f3,f4],Subscript[wwC, lq3][f1,f2,f3,f4],Subscript[wwC, ee][f1,f2,f3,f4],Subscript[wwC, dd][f1,f2,f3,f4],Subscript[wwC, uu][f1,f2,f3,f4],Subscript[wwC, ed][f1,f2,f3,f4],Subscript[wwC, eu][f1,f2,f3,f4],Subscript[wwC, ud1][f1,f2,f3,f4],Subscript[wwC, ud8][f1,f2,f3,f4],Subscript[wwC, le][f1,f2,f3,f4],Subscript[wwC, ld][f1,f2,f3,f4],Subscript[wwC, lu][f1,f2,f3,f4],Subscript[wwC, qe][f1,f2,f3,f4],Subscript[wwC, qu1][f1,f2,f3,f4],Subscript[wwC, qu8][f1,f2,f3,f4],Subscript[wwC, qd1][f1,f2,f3,f4],Subscript[wwC, qd8][f1,f2,f3,f4],Subscript[wwC, ledq][f1,f2,f3,f4],
Subscript[wwC, quqd1][f1,f2,f3,f4],Subscript[wwC, quqd8][f1,f2,f3,f4],Subscript[wwC, lequ1][f1,f2,f3,f4],Subscript[wwC, lequ3][f1,f2,f3,f4],Subscript[wwC, duq][f1,f2,f3,f4],Subscript[wwC, qqu][f1,f2,f3,f4],Subscript[wwC, qqq][f1,f2,f3,f4],Subscript[wwC, duu][f1,f2,f3,f4],Subscript[wwC, W],Subscript[wwC, Wtil],Subscript[wwC, G],Subscript[wwC, Gtil],Subscript[wwC, \[Phi]],Subscript[wwC, \[Phi]D],Subscript[wwC, \[Phi]Sq],Subscript[wwC, \[Phi]B],Subscript[wwC, \[Phi]Btil],Subscript[wwC, \[Phi]W],Subscript[wwC, \[Phi]Wtil],Subscript[wwC, \[Phi]WB],Subscript[wwC, \[Phi]WBtil],Subscript[wwC, \[Phi]G],Subscript[wwC, \[Phi]Gtil],Subscript[wwC, e\[Phi]][f1,f2],Subscript[wwC, d\[Phi]][f1,f2],Subscript[wwC, u\[Phi]][f1,f2],Subscript[wwC, eB][f1,f2],Subscript[wwC, eW][f1,f2],Subscript[wwC, dB][f1,f2],Subscript[wwC, uB][f1,f2],Subscript[wwC, uW][f1,f2],Subscript[wwC, dW][f1,f2],Subscript[wwC, dG][f1,f2],Subscript[wwC, uG][f1,f2],Subscript[wwC, \[Phi]L1][f1,f2],Subscript[wwC, \[Phi]L3][f1,f2],Subscript[wwC, \[Phi]q1][f1,f2],Subscript[wwC, \[Phi]q3][f1,f2],Subscript[wwC, \[Phi]e][f1,f2],Subscript[wwC, \[Phi]d][f1,f2],Subscript[wwC, \[Phi]u][f1,f2],Subscript[wwC, \[Phi]ud][f1,f2]};
nameListOpeNoHRenorm[f1_,f2_,f3_,f4_]:={Z\[Phi]*Subscript[wC, 5][f1,f2],Subscript[wC, ll][f1,f2,f3,f4],Subscript[wC, qq1][f1,f2,f3,f4],Subscript[wC, qq3][f1,f2,f3,f4],Subscript[wC, lq1][f1,f2,f3,f4],Subscript[wC, lq3][f1,f2,f3,f4],Subscript[wC, ee][f1,f2,f3,f4],Subscript[wC, dd][f1,f2,f3,f4],Subscript[wC, uu][f1,f2,f3,f4],Subscript[wC, ed][f1,f2,f3,f4],Subscript[wC, eu][f1,f2,f3,f4],Subscript[wC, ud1][f1,f2,f3,f4],Subscript[wC, ud8][f1,f2,f3,f4],Subscript[wC, le][f1,f2,f3,f4],Subscript[wC, ld][f1,f2,f3,f4],Subscript[wC, lu][f1,f2,f3,f4],Subscript[wC, qe][f1,f2,f3,f4],Subscript[wC, qu1][f1,f2,f3,f4],Subscript[wC, qu8][f1,f2,f3,f4],Subscript[wC, qd1][f1,f2,f3,f4],Subscript[wC, qd8][f1,f2,f3,f4],Subscript[wC, ledq][f1,f2,f3,f4],
Subscript[wC, quqd1][f1,f2,f3,f4],Subscript[wC, quqd8][f1,f2,f3,f4],Subscript[wC, lequ1][f1,f2,f3,f4],Subscript[wC, lequ3][f1,f2,f3,f4],Subscript[wC, duq][f1,f2,f3,f4],Subscript[wC, qqu][f1,f2,f3,f4],Subscript[wC, qqq][f1,f2,f3,f4],Subscript[wC, duu][f1,f2,f3,f4],Subscript[wC, W],Subscript[wC, Wtil],Subscript[wC, G],Subscript[wC, Gtil],Z\[Phi]^3*Subscript[wC, \[Phi]],Z\[Phi]^2*Subscript[wC, \[Phi]D],Z\[Phi]^2*Subscript[wC, \[Phi]Sq],Z\[Phi] Subscript[wC, \[Phi]B],Z\[Phi] Subscript[wC, \[Phi]Btil],Z\[Phi] Subscript[wC, \[Phi]W],Z\[Phi] Subscript[wC, \[Phi]Wtil],Z\[Phi] Subscript[wC, \[Phi]WB],Z\[Phi] Subscript[wC, \[Phi]WBtil],Z\[Phi] Subscript[wC, \[Phi]G],Z\[Phi] Subscript[wC, \[Phi]Gtil],(Z\[Phi] ^(3/2))*Subscript[wC, e\[Phi]][f1,f2],(Z\[Phi] ^(3/2))*Subscript[wC, d\[Phi]][f1,f2],(Z\[Phi] ^(3/2))*Subscript[wC, u\[Phi]][f1,f2],Sqrt[Z\[Phi]]*Subscript[wC, eB][f1,f2],Sqrt[Z\[Phi]]*Subscript[wC, eW][f1,f2],Sqrt[Z\[Phi]]*Subscript[wC, dB][f1,f2],Sqrt[Z\[Phi]]*Subscript[wC, uB][f1,f2],Sqrt[Z\[Phi]]*Subscript[wC, uW][f1,f2],Sqrt[Z\[Phi]]*Subscript[wC, dW][f1,f2],Sqrt[Z\[Phi]]*Subscript[wC, dG][f1,f2],Sqrt[Z\[Phi]]*Subscript[wC, uG][f1,f2],Z\[Phi] Subscript[wC, \[Phi]L1][f1,f2],Z\[Phi] Subscript[wC, \[Phi]L3][f1,f2],Z\[Phi] Subscript[wC, \[Phi]q1][f1,f2],Z\[Phi] Subscript[wC, \[Phi]q3][f1,f2],Z\[Phi] Subscript[wC, \[Phi]e][f1,f2],Z\[Phi] Subscript[wC, \[Phi]d][f1,f2],Z\[Phi] Subscript[wC, \[Phi]u][f1,f2],Z\[Phi] Subscript[wC, \[Phi]ud][f1,f2],gHat[B][\[Phi]][f1],gHat[B1][\[Phi]][f1],gHat[W][\[Phi]][f1],gHat[W1][\[Phi]][f1],Sqrt[Z\[Phi]]*Subscript[wC, y][e][f1,f2],Sqrt[Z\[Phi]]*Subscript[wC, y][d][f1,f2],Sqrt[Z\[Phi]]*Subscript[wC, y][u][f1,f2],Z\[Phi]^2*Subscript[wC, \[Phi]4],Z\[Phi]};
nameListModCoup:={gHat[B][\[Phi]][f1],gHat[B1][\[Phi]][f1],gHat[W][\[Phi]][f1],gHat[W1][\[Phi]][f1],yHat[e][f1,f2],yHat[d][f1,f2],yHat[u][f1,f2],\[Lambda]Hat[\[Phi]],Subscript[wC, y][e][f1,f2],Subscript[wC, y][d][f1,f2],Subscript[wC, y][u][f1,f2],\[Mu]Hat[\[Phi]],Subscript[wC, \[Phi]4],Z\[Phi]};
nameListModCoupNoEval:={ggHat[B][\[Phi]][f1],ggHat[B1][\[Phi]][f1],ggHat[W][\[Phi]][f1],ggHat[W1][\[Phi]][f1],yyHat[e][f1,f2],yyHat[d][f1,f2],yyHat[u][f1,f2],\[Lambda]\[Lambda]Hat[\[Phi]],Subscript[wwC, y][e][f1,f2],Subscript[wwC, y][d][f1,f2],Subscript[wwC, y][u][f1,f2],\[Mu]\[Mu]Hat[\[Phi]],Subscript[wwC, \[Phi]4],zZ\[Phi]};
nameListModSMCoup:={Subscript[wC, y][e][f1,f2],Subscript[wC, y][d][f1,f2],Subscript[wC, y][u][f1,f2],Subscript[wC, \[Phi]4]};
nameListModSMCoupNoEval:={Subscript[wwC, y][e][f1,f2],Subscript[wwC, y][d][f1,f2],Subscript[wwC, y][u][f1,f2],Subscript[wwC, \[Phi]4]};
nameListOperatorsCheck[f1_,f2_,f3_,f4_]:=Join[nameListOperators[f1,f2,f3,f4],nameListModSMCoup];
nameListOperatorsCheckNoEval[f1_,f2_,f3_,f4_]:=Join[nameListOperatorsNoEval[f1,f2,f3,f4],nameListModSMCoupNoEval]
listReempAllReal={Conjugate[m]->m,Conjugate[y[\[CapitalXi]1][r_,i_,j_]]:>y[\[CapitalXi]1][r,i,j],Conjugate[g[B][u][r_,f1_,f2_]]:>g[B][u][r,f1,f2],Conjugate[\[Gamma][L1][r_]]:>\[Gamma][L1][r],Conjugate[g[G][u][r_,f1_,f2_]]:>g[G][u][r,f1,f2],Conjugate[\[Delta][L1,\[CurlyPhi]][t_,s_]]:>\[Delta][L1,\[CurlyPhi]][t,s],Conjugate[y[S1][r_,i_,j_]]:>y[S1][r,i,j],Conjugate[y[\[Zeta]][qq][r_,i_,j_]]:>y[\[Zeta]][qq][r,i,j],Conjugate[y[\[CapitalUpsilon]][r_,i_,j_]]:>y[\[CapitalUpsilon]][r,i,j],Conjugate[y[\[Omega]1][qq][r_,i_,j_]]:>y[\[Omega]1][qq][r,i,j],Conjugate[y[\[CapitalOmega]1][qq][r_,i_,j_]]:>y[\[CapitalOmega]1][qq][r,i,j],Conjugate[g[X][r_,i_,j_]]:>g[X][r,i,j],Conjugate[g[\[ScriptCapitalU]2][Lq][r_,i_,j_]]:>g[\[ScriptCapitalU]2][Lq][r,i,j],Conjugate[y[\[Zeta]][qL][r_,i_,j_]]:>y[\[Zeta]][qL][r,i,j],Conjugate[y[\[Omega]1][qL][r_,i_,j_]]:>y[\[Omega]1][qL][r,i,j],Conjugate[y[S2][r_,i_,j_]]:>y[S2][r,i,j],Conjugate[y[\[Omega]2][r_,i_,j_]]:>y[\[Omega]2][r,i,j],Conjugate[y[\[CapitalOmega]2][r_,i_,j_]]:>y[\[CapitalOmega]2][r,i,j],Conjugate[y[\[CapitalOmega]4][r_,i_,j_]]:>y[\[CapitalOmega]4][r,i,j],Conjugate[y[\[Omega]4][uu][r_,i_,j_]]:>y[\[Omega]4][uu][r,i,j],Conjugate[g[\[ScriptCapitalU]2][ed][r_,i_,j_]]:>g[\[ScriptCapitalU]2][ed][r,i,j],Conjugate[y[\[Omega]4][ed][r_,i_,j_]]:>y[\[Omega]4][ed][r,i,j],Conjugate[g[\[ScriptCapitalU]5][ed][r_,i_,j_]]:>g[\[ScriptCapitalU]5][ed][r,i,j],Conjugate[y[\[Omega]1][eu][r_,i_,j_]]:>y[\[Omega]1][eu][r,i,j],Conjugate[g[G1][r_,i_,j_]]:>g[G1][r,i,j],Conjugate[g[B1][du][r_,i_,j_]]:>g[B1][du][r,i,j],Conjugate[y[\[Omega]1][du][r_,i_,j_]]:>y[\[Omega]1][du][r,i,j],Conjugate[y[\[CapitalOmega]1][ud][r_,i_,j_]]:>y[\[CapitalOmega]1][ud][r,i,j],Conjugate[g[L3][r_,i_,j_]]:>g[L3][r,i,j],Conjugate[y[\[CurlyPhi]][e][r_,i_,j_]]:>y[\[CurlyPhi]][e][r,i,j],Conjugate[\[Delta][L1,\[CurlyPhi]][s_,r_]]:>\[Delta][L1,\[CurlyPhi]][s,r],Conjugate[Subscript[\[Mu], \[Phi]]]:>Subscript[\[Mu], \[Phi]],Conjugate[\!\(\*SubsuperscriptBox[\(\[Mu]\), \(\[Phi]\), \(2\)]\)]:>\!\(\*SubsuperscriptBox[\(\[Mu]\), \(\[Phi]\), \(2\)]\),Conjugate[y[e][j_,i_]]:>y[e][j,i],Conjugate[y[\[CapitalPi]1][r_,i_,j_]]:>y[\[CapitalPi]1][r,i,j],Conjugate[g[\[ScriptCapitalQ]5][dL][r_,i_,j_]]:>g[\[ScriptCapitalQ]5][dL][r,i,j],Conjugate[g[\[ScriptCapitalQ]1][uL][r_,i_,j_]]:>g[\[ScriptCapitalQ]1][uL][r,i,j],Conjugate[y[\[CapitalPi]7][Lu][r_,i_,j_]]:>y[\[CapitalPi]7][Lu][r,i,j],Conjugate[g[\[ScriptCapitalQ]5][eq][r_,i_,j_]]:>g[\[ScriptCapitalQ]5][eq][r,i,j],Conjugate[y[\[CapitalPi]7][eq][r_,i_,j_]]:>y[\[CapitalPi]7][eq][r,i,j],Conjugate[g[Y5][r_,i_,j_]]:>g[Y5][r,i,j],Conjugate[g[\[ScriptCapitalQ]5][uq][r_,i_,j_]]:>g[\[ScriptCapitalQ]5][uq][r,i,j],Conjugate[y[\[CurlyPhi]][u][r_,i_,j_]]:>y[\[CurlyPhi]][u][r,i,j],Conjugate[y[\[CapitalPhi]][qu][r_,i_,j_]]:>y[\[CapitalPhi]][qu][r,i,j],Conjugate[y[u][i_,j_]]:>y[u][i,j],Conjugate[\[Delta][L1,\[CurlyPhi]][r_,s_]]:>\[Delta][L1,\[CurlyPhi]][r,s],Conjugate[y[\[CurlyPhi]][u][s_,i_,j_]]:>y[\[CurlyPhi]][u][s,i,j],Conjugate[g[Y1][r_,i_,j_]]:>g[Y1][r,i,j],Conjugate[g[\[ScriptCapitalQ]1][dq][r_,i_,j_]]:>g[\[ScriptCapitalQ]1][dq][r,i,j],Conjugate[y[\[CurlyPhi]][d][r_,i_,j_]]:>y[\[CurlyPhi]][d][r,i,j],Conjugate[y[\[CapitalPhi]][dq][r_,i_,j_]]:>y[\[CapitalPhi]][dq][r,i,j],Conjugate[y[\[CurlyPhi]][d][s_,i_,j_]]:>y[\[CurlyPhi]][d][s,i,j],Conjugate[y[d][j_,i_]]:>y[d][j,i],Conjugate[g[\[ScriptCapitalQ]1][dq][r_,i_,j_]*g[\[ScriptCapitalQ]1][uL][s_,k_,l_]]:>g[\[ScriptCapitalQ]1][dq][r,i,j] g[\[ScriptCapitalQ]1][uL][s,k,l],Conjugate[g[\[ScriptCapitalQ]5][eq][r_,i_,j_] g[\[ScriptCapitalQ]5][uq][s_,k_,l_]]:>g[\[ScriptCapitalQ]5][eq][r,i,j] g[\[ScriptCapitalQ]5][uq][s,k,l],Conjugate[y[\[Zeta]][qL][r_,i_,j_]*y[\[Zeta]][qq][s_,k_,l_]]:>y[\[Zeta]][qL][r,i,j] y[\[Zeta]][qq][s,k,l],Conjugate[\[Lambda][\[CapitalTheta]1][r_]]:>\[Lambda][\[CapitalTheta]1][r],Conjugate[\[Lambda][\[CapitalTheta]3][r_]]:>\[Lambda][\[CapitalTheta]3][r],Conjugate[\[Lambda][\[CurlyPhi]][r_]]:>\[Lambda][\[CurlyPhi]][r],Conjugate[w[S,Q1][r_,s_,t_]]:>w[S,Q1][r,s,t],Conjugate[\[Zeta][L1,B1][s_,r_]]:>\[Zeta][L1,B1][s,r],Conjugate[g[B1][\[Phi]][r_]]:>g[B1][\[Phi]][r],Conjugate[\[Zeta][L1,W][s_,r_]]:>\[Zeta][L1,W][s,r],Conjugate[g[W][\[Phi]][r_]]:>g[W][\[Phi]][r],Conjugate[\[Zeta][L1,W1][s_,r_]]:>\[Zeta][L1,W1][s,r],Conjugate[g[W1][\[Phi]][r_]]:>g[W1][\[Phi]][r],Conjugate[\[Kappa][\[CapitalXi]1][r_]]:>\[Kappa][\[CapitalXi]1][r],Conjugate[\[Lambda][\[CapitalTheta]1][s_]]:>\[Lambda][\[CapitalTheta]1][s],Conjugate[\[Lambda][\[CapitalTheta]3][s_]]:>\[Lambda][\[CapitalTheta]3][s],Conjugate[\[Kappa][\[CapitalXi]1,\[CurlyPhi]][r_,s_]]:>\[Kappa][\[CapitalXi]1,\[CurlyPhi]][r,s],Conjugate[\[Lambda][\[CurlyPhi]][s_]]:>\[Lambda][\[CurlyPhi]][s],Conjugate[\[Kappa][\[CapitalXi]1][s_]]:>\[Kappa][\[CapitalXi]1][s],Conjugate[\[Kappa][\[CapitalXi],\[CurlyPhi]][r_,s_]]:>\[Kappa][\[CapitalXi],\[CurlyPhi]][r,s],Conjugate[\[Kappa][\[CapitalXi]1,\[CapitalTheta]1][r_,s_]]:>\[Kappa][\[CapitalXi]1,\[CapitalTheta]1][r,s],Conjugate[\[Kappa][\[CapitalXi],\[CurlyPhi]][t_,s_]]:>\[Kappa][\[CapitalXi],\[CurlyPhi]][t,s],Conjugate[\[Kappa][S,\[CurlyPhi]][r_,t_]]:>\[Kappa][S,\[CurlyPhi]][r,t],Conjugate[\[Kappa][\[CapitalXi],\[CapitalTheta]1][r_,s_]]:>\[Kappa][\[CapitalXi],\[CapitalTheta]1][r,s],Conjugate[\[Kappa][\[CapitalXi],\[CurlyPhi]][t_,r_]]:>\[Kappa][\[CapitalXi],\[CurlyPhi]][t,r],Conjugate[\[Kappa][\[CapitalXi]1,\[CapitalTheta]1][t_,r_]]:>\[Kappa][\[CapitalXi]1,\[CapitalTheta]1][t,r],Conjugate[\[Kappa][\[CapitalXi]1,\[CapitalTheta]3][t_,r_]]:>\[Kappa][\[CapitalXi]1,\[CapitalTheta]3][t,r],Conjugate[\[Kappa][\[CapitalXi]1,\[CurlyPhi]][s_,t_]]:>\[Kappa][\[CapitalXi]1,\[CurlyPhi]][s,t],Conjugate[g1[\[CapitalXi]1,L1][r_,s_]]:>g1[\[CapitalXi]1,L1][r,s],Conjugate[g1[\[CapitalXi],L1][r_,s_]]:>g1[\[CapitalXi],L1][r,s],Conjugate[\[Kappa][\[CapitalXi]1,\[CurlyPhi]][t_,s_]]:>\[Kappa][\[CapitalXi]1,\[CurlyPhi]][t,s],Conjugate[\[Delta][W1,\[CapitalXi]1][s_,t_]]:>\[Delta][W1,\[CapitalXi]1][s,t],Conjugate[\[Kappa][\[CapitalXi]1][t_]]:>\[Kappa][\[CapitalXi]1][t],Conjugate[\[Zeta][L1,B][s_,r_]]:>\[Zeta][L1,B][s,r],Conjugate[g[B][\[Phi]][r_]]:>g[B][\[Phi]][r],Conjugate[\[CurlyEpsilon][\[CapitalXi]1,L1][r_,s_,t_]]:>\[CurlyEpsilon][\[CapitalXi]1,L1][r,s,t],Conjugate[g1[S,L1][r_,s_]]:>g1[S,L1][r,s],Conjugate[g2[S,L1][r_,s_]]:>g2[S,L1][r,s],Conjugate[\[CurlyEpsilon][\[CapitalXi],L1][r_,s_,t_]]:>\[CurlyEpsilon][\[CapitalXi],L1][r,s,t],Conjugate[\[Lambda][Ef][r_,i_]]:>\[Lambda][Ef][r,i],Conjugate[\[Lambda][Ne][r_,i_]]:>\[Lambda][Ne][r,i],Conjugate[\[Lambda][\[CapitalSigma]][r_,i_]]:>\[Lambda][\[CapitalSigma]][r,i],Conjugate[\[Lambda][\[CapitalSigma]1][r_,i_]]:>\[Lambda][\[CapitalSigma]1][r,i],Conjugate[z[Ef,L1][r_,s_,i_]]:>z[Ef,L1][r,s,i],Conjugate[z[Ne,L1][r_,s_,i_]]:>z[Ne,L1][r,s,i],Conjugate[z[\[CapitalSigma],L1][s_,r_,i_]]:>z[\[CapitalSigma],L1][s,r,i],Conjugate[z[\[CapitalSigma]1,L1][r_,s_,i_]]:>z[\[CapitalSigma]1,L1][r,s,i],Conjugate[\[Lambda][Df][r_,i_]]:>\[Lambda][Df][r,i],Conjugate[\[Lambda][T1][r_,i_]]:>\[Lambda][T1][r,i],Conjugate[\[Lambda][T2][r_,i_]]:>\[Lambda][T2][r,i],Conjugate[\[Lambda][U][r_,i_]]:>\[Lambda][U][r,i],Conjugate[z[Df,L1][r_,s_,i_]]:>z[Df,L1][r,s,i],Conjugate[z[T1,L1][s_,r_,i_]]:>z[T1,L1][s,r,i],Conjugate[z[T2,L1][s_,r_,i_]]:>z[T2,L1][s,r,i],Conjugate[z[U,L1][r_,s_,i_]]:>z[U,L1][r,s,i],Conjugate[\[Lambda][\[CapitalDelta]1][r_,i_]]:>\[Lambda][\[CapitalDelta]1][r,i],Conjugate[\[Lambda][\[CapitalDelta]3][r_,i_]]:>\[Lambda][\[CapitalDelta]3][r,i],Conjugate[z[\[CapitalDelta]1,L1][r_,s_,i_]]:>z[\[CapitalDelta]1,L1][r,s,i],Conjugate[z[\[CapitalDelta]3,L1][s_,r_,i_]]:>z[\[CapitalDelta]3,L1][s,r,i],Conjugate[\[Lambda][Q5][r_,i_]]:>\[Lambda][Q5][r,i],Conjugate[\[Lambda][Q1][d][r_,i_]]:>\[Lambda][Q1][d][r,i],Conjugate[z[Q5,L1][r_,s_,i_]]:>z[Q5,L1][r,s,i],Conjugate[z[Q1,L1][d][r_,s_,i_]]:>z[Q1,L1][d][r,s,i],Conjugate[\[Lambda][Q7][r_,i_]]:>\[Lambda][Q7][r,i],Conjugate[\[Lambda][Q1][u][r_,i_]]:>\[Lambda][Q1][u][r,i],Conjugate[z[Q7,L1][r_,s_,i_]]:>z[Q7,L1][r,s,i],Conjugate[\[Lambda][Q1][u][s_,i_]]:>\[Lambda][Q1][u][s,i],Conjugate[z[Q1,L1][u][s_,r_,i_]]:>z[Q1,L1][u][s,r,i],
Im[Sum[(\[Gamma][L1][s] * \[Zeta][L1, B][s, r_])/(M[L1][s]^2),{s,1,a_}]]:>0,Conjugate[Subscript[\[Mu], \[Phi]]]^2:>Subscript[\[Mu], \[Phi]]^2 };
listSMcouplings={Subscript[g1, SM],Subscript[g2, SM],\[Lambda]Hat[\[Phi]],Subscript[\[Mu], \[Phi]],\[Lambda]\[Phi]};
listAllUVCouplings={g[G1][r,f3,f2],g[H][r,f1,f2],g[L3][r,f3,f1],g[X][r,f1,f4],g[Y1][r,f3,f1],g[Y5][r,f3,f1],g[\[CapitalXi]1,L1][r,s],g1[L1][r,s],g1[S,L1][r,s],g1[\[CapitalXi],L1][r,s],g1[\[CapitalXi]1,L1][r,s],g2[S,L1][r,s],g2[\[CapitalXi],L1][r,s],g2[\[CapitalXi]1,L1][r,s],h1[L1][r,s],h2[L1][r,s],h3[L1][r,s],w[S,Df][r,s,f2],w[S,Ef][r,s,f2],w[S,Q1][r,s,f1],w[S,U][r,s,f2],w[S,\[CapitalDelta]1][r,s,f1],w[\[CapitalXi],Q1][s,r,f1],w[\[CapitalXi],Q7][r,s,f1],w[\[CapitalXi],T1][r,s,f2],w[\[CapitalXi],T2][s,r,f2],w[\[CapitalXi],\[CapitalDelta]3][r,s,f1],w[\[CapitalXi],\[CapitalSigma]1][s,r,f2],w[\[CapitalXi]1,Q5][r,s,f1],w[\[CapitalXi]1,Q7][r,s,f1],w[\[CapitalXi]1,T2][r,s,f2],w[\[CapitalXi]1,\[CapitalDelta]3][r,s,f1],w[\[CapitalXi]1,\[CapitalSigma]][r,s,f2],y[S1][r,f1,f3],y[S2][r,f3,f1],y[\[CapitalXi]1][r,f2,f1],y[\[CapitalPi]1][r,f1,f4],y[\[CapitalUpsilon]][r,f2,f4],y[\[Omega]2][r,f3,f1],y[\[CapitalOmega]2][r,f1,f3],y[\[CapitalOmega]4][r,f1,f3],z[Df,L1][r,s,f1],z[Df,L1][r,s,k],z[Ef,L1][r,s,f1],z[Ef,L1][r,s,k],z[Ne,L1][r,s,f1],z[Q5,L1][r,s,f1],z[Q5,L1][r,s,k],z[Q7,L1][r,s,f1],z[Q7,L1][r,s,k],z[T1,L1][r,s,f2],z[T1,L1][r,s,k],z[T2,L1][r,s,f2],z[T2,L1][r,s,k],z[U,L1][r,s,f1],z[U,L1][r,s,k],z[\[CapitalDelta]1,L1][r,s,f1],z[\[CapitalDelta]1,L1][r,s,k],z[\[CapitalDelta]3,L1][s,r,f1],z[\[CapitalDelta]3,L1][s,r,k],z[\[CapitalSigma],L1][r,s,f2],z[\[CapitalSigma],L1][r,s,k],z[\[CapitalSigma]1,L1][r,s,f1],z[\[CapitalSigma]1,L1][r,s,k],\[Gamma][L1][r],\[Delta][B,S][r,s],\[Delta][L1,\[CurlyPhi]][r,s],\[Delta][W,\[CapitalXi]][r,s],\[Delta][W1,\[CapitalXi]1][r,s],\[CurlyEpsilon][S,L1][r,t,s],\[CurlyEpsilon][\[CapitalXi],L1][r,s,t],\[CurlyEpsilon][\[CapitalXi]1,L1][r,s,t],\[Zeta][L1,B][s,f1],\[Zeta][L1,B][s,r],\[Zeta][L1,B1][s,f1],\[Zeta][L1,B1][s,r],\[Zeta][L1,W][s,f1],\[Zeta][L1,W][s,r],\[Zeta][L1,W1][s,f1],\[Zeta][L1,W1][s,r],\[Kappa][S][r],\[Kappa][\[CapitalXi]][r],\[Kappa][\[CapitalXi]1][r],\[Kappa][S,\[CapitalXi]][t,s,r],\[Kappa][S,\[CapitalXi]1][t,r,s],\[Kappa][S,\[CurlyPhi]][r,s],\[Kappa][\[CapitalXi],\[CapitalTheta]1][r,s],\[Kappa][\[CapitalXi],\[CapitalXi]1][s,r,t],\[Kappa][\[CapitalXi],\[CurlyPhi]][r,s],\[Kappa][\[CapitalXi]1,\[CapitalTheta]1][r,s],\[Kappa][\[CapitalXi]1,\[CapitalTheta]3][r,s],\[Kappa][\[CapitalXi]1,\[CurlyPhi]][r,s],\[Kappa]3[S][r,t,s],\[Lambda][Df][r,f1],\[Lambda][Df][r,k],\[Lambda][Ef][r,f1],\[Lambda][Ef][r,k],\[Lambda][Ne][r,f1],\[Lambda][Q5][r,f1],\[Lambda][Q5][r,k],\[Lambda][Q7][r,f1],\[Lambda][Q7][r,k],\[Lambda][S][r,s],\[Lambda][T1][r,f1],\[Lambda][T1][r,k],\[Lambda][T2][r,f1],\[Lambda][T2][r,k],\[Lambda][U][r,f1],\[Lambda][U][r,k],\[Lambda][\[CapitalDelta]1][r,f1],\[Lambda][\[CapitalDelta]1][r,k],\[Lambda][\[CapitalDelta]3][r,f1],\[Lambda][\[CapitalDelta]3][r,k],\[Lambda][\[CapitalTheta]1][r],\[Lambda][\[CapitalTheta]3][r],\[Lambda][\[CapitalXi]][s],\[Lambda][\[CapitalXi]][s,r],\[Lambda][\[CapitalXi]1][r,s],\[Lambda][\[CapitalSigma]][r,f1],\[Lambda][\[CapitalSigma]][r,k],\[Lambda][\[CapitalSigma]1][r,f1],\[Lambda][\[CapitalSigma]1][r,k],\[Lambda][\[CurlyPhi]][r],\[Lambda][Df,Q1][s,r],\[Lambda][Df,Q5][r,s],\[Lambda][Ef,\[CapitalDelta]1][r,s],\[Lambda][Ef,\[CapitalDelta]3][s,r],\[Lambda][S,\[CapitalXi]1][r,s],\[Lambda][T1,Q1][r,s],\[Lambda][T1,Q5][r,s],\[Lambda][T2,Q1][s,r],\[Lambda][T2,Q7][s,r],\[Lambda][U,Q1][r,s],\[Lambda][U,Q7][r,s],\[Lambda][\[CapitalXi]1,\[CapitalXi]][r,s],\[Lambda][\[CapitalSigma],\[CapitalDelta]1][s,r],\[Lambda][\[CapitalSigma]1,\[CapitalDelta]1][r,s],\[Lambda][\[CapitalSigma]1,\[CapitalDelta]3][s,r],\[Lambda]p[\[CapitalXi]1][r,s],g[B][d][r,f1,f2],g[B][d][r,k,f2],g[B][e][r,f1,f2],g[B][e][r,k,f2],g[B][L][r,f1,f2],g[B][L][r,f1,k],g[B][q][r,f1,f2],g[B][q][r,f1,k],g[B][u][r,f1,f2],g[B][u][r,k,f2],g[B][\[Phi]][f1],g[B][\[Phi]][r],g[B1][du][r,f2,f1],g[B1][\[Phi]][f1],g[B1][\[Phi]][r],g[G][d][r,f1,f2],g[G][q][r,f1,f2],g[G][u][r,f1,f2],g[L1][B][r,s],g[L1][Btil][r,s],g[L1][W][s,r],g[L1][Wsm][s,r],g[L1][Wtil][r,s],g[W][L][r,f1,f2],g[W][L][r,f1,k],g[W][q][r,f1,f2],g[W][q][r,f1,k],g[W][\[Phi]][f1],g[W][\[Phi]][r],g[W1][\[Phi]][f1],g[W1][\[Phi]][r],g[\[ScriptCapitalQ]1][dq][r,f1,f3],g[\[ScriptCapitalQ]1][uL][r,f2,f4],g[\[ScriptCapitalQ]5][dL][r,f1,f4],g[\[ScriptCapitalQ]5][eq][r,f2,f4],g[\[ScriptCapitalQ]5][uq][r,f2,f3],g[\[ScriptCapitalU]2][ed][r,f1,f4],g[\[ScriptCapitalU]2][Lq][r,f1,f4],g[\[ScriptCapitalU]5][ed][r,f1,f4],y[\[Zeta]][qL][r,f3,f1],y[\[Zeta]][qq][r,f1,f2],y[\[CapitalPi]7][eq][r,f1,f4],y[\[CapitalPi]7][Lu][r,f1,f4],y[\[CapitalUpsilon]][qq][r,f4,f2],y[\[CurlyPhi]][d][r,f1,f2],y[\[CurlyPhi]][d][r,f2,k],y[\[CurlyPhi]][d][r,k,f1],y[\[CurlyPhi]][e][r,f1,f2],y[\[CurlyPhi]][e][r,f2,k],y[\[CurlyPhi]][e][r,k,f1],y[\[CurlyPhi]][u][r,f1,f2],y[\[CurlyPhi]][u][r,f2,k],y[\[CurlyPhi]][u][r,k,f1],y[\[CapitalPhi]][dq][r,f3,f2],y[\[CapitalPhi]][qu][r,f1,f2],y[\[Omega]1][du][r,f1,f2],y[\[Omega]1][ed][r,f2,f4],y[\[Omega]1][eu][r,f1,f3],y[\[Omega]1][qL][r,f3,f1],y[\[Omega]1][qq][r,f1,f2],y[\[CapitalOmega]1][qq][r,f1,f3],y[\[CapitalOmega]1][ud][r,f1,f3],y[\[Omega]4][ed][r,f1,f3],y[\[Omega]4][uu][r,f2,f3],z[Q1,L1][d][r,s,f1],z[Q1,L1][d][r,s,k],z[Q1,L1][u][s,r,f1],z[Q1,L1][u][s,r,k],\[Lambda][Q1][d][r,f1],\[Lambda][Q1][d][r,k],\[Lambda][Q1][u][r,f1],\[Lambda][Q1][u][r,k]};
allUVCouplingsSimplified={\[Lambda]\[Phi]->lam,
Subscript[\[Mu], \[Phi]]->muH,
g[G1][r_,f3_,f2_]:>gG1f[f3,f2],
g[H][r_,f1_,f2_]:>gHf[f1,f2],
g[L3][r_,f3_,f1_]:>gL3f[f3,f1],
g[X][r_,f1_,f4_]:>gXf[f1,f4],
g[Y1][r_,f3_,f1_]:>gY1f[f3,f1],
g[Y5][r_,f3_,f1_]:>gY5f[f3,f1],
g[\[CapitalXi]1,L1][r_,s_]:>gX1L1,
g1[L1][r_,s_]:>g1L1,
g1[S,L1][r_,s_]:> g1SL1,
g1[\[CapitalXi],L1][r_,s_]:>g1XiL1,
g1[\[CapitalXi]1,L1][r_,s_]:>g1Xi1L1,
g2[S,L1][r_,s_]:>g2SL1,
g2[\[CapitalXi],L1][r_,s_]:>g2XiL1,
g2[\[CapitalXi]1,L1][r_,s_]:>g2Xi1L1,
h1[L1][r_,s_]:>h1L1,
h2[L1][r_,s_]:>h2L1,
h3[L1][r_,s_]:>h3L1,
w[S,Df][r_,s_,f2_]:>wSDff[f2],
w[S,Ef][r_,s_,f2_]:>wSEff[f2],
w[S,Q1][r_,s_,f1_]:>wSQ1f[f1],
w[S,U][r_,s_,f2_]:>wSUf[f2],
w[S,\[CapitalDelta]1][r_,s_,f1_]:>wSDelta1f[f1],
w[\[CapitalXi],Q1][s_,r_,f1_]:>wXiQ1f[f1],
w[\[CapitalXi],Q7][r_,s_,f1_]:>wXiQ7f[f1],
w[\[CapitalXi],T1][r_,s_,f2_]:>wXiT1f[f2],
w[\[CapitalXi],T2][s_,r_,f2_]:>wXiT2f[f2],
w[\[CapitalXi],\[CapitalDelta]3][r_,s_,f1_]:>wXiDelta3f[f1],
w[\[CapitalXi],\[CapitalSigma]1][s_,r_,f2_]:>wXiSigma1f[f2],
w[\[CapitalXi]1,Q5][r_,s_,f1_]:>wXi1Q5f[f1],
w[\[CapitalXi]1,Q7][r_,s_,f1_]:>wXi1Q7f[f1],
w[\[CapitalXi]1,T2][r_,s_,f2_]:>wXiT2f[f2],
w[\[CapitalXi]1,\[CapitalDelta]3][r_,s_,f1_]:>wXi1Delta3f[f1],
w[\[CapitalXi]1,\[CapitalSigma]][r_,s_,f2_]:>wXi1Sigmaf[f2],
y[S1][r_,f1_,f3_]:>yS1f[f1,f3],
y[S2][r_,f3_,f1_]:>yS2f[f3,f1],
y[\[CapitalXi]1][r_,f2_,f1_]:>ySigma1f[f2,f1],
y[\[CapitalPi]1][r_,f1_,f4_]:>yPi1f[f1,f4],
y[\[CapitalUpsilon]][r_,f2_,f4_]:>yUpsf[f2,f4],
y[\[Omega]2][r_,f3_,f1_]:>yomega2f[f3,f1],
y[\[CapitalOmega]2][r_,f1_,f3_]:>yOmega2f[f1,f3],
y[\[CapitalOmega]4][r_,f1_,f3_]:>yOmega4f[f1,f3],
z[Df,L1][r_,s_,f1_]:>zDfL1f[f1],
z[Ef,L1][r_,s_,f1_]:>zEfL1f[f1],
z[Ne,L1][r_,s_,f1_]:>zNeL1f[f1],
z[Q5,L1][r_,s_,f1_]:>zQ5L1f[f1],
z[Q7,L1][r_,s_,f1_]:>zQ7L1f[f1],
z[T1,L1][r_,s_,f2_]:>zT1L1f[f2],
z[T2,L1][r_,s_,f2_]:>zT2L1f[f2],
z[U,L1][r_,s_,f1_]:>zUL1f[f1],
z[\[CapitalDelta]1,L1][r_,s_,f1_]:>zDelta1L1f[f1],
z[\[CapitalDelta]3,L1][s_,r_,f1_]:>zDelta3L1f[f1],
z[\[CapitalSigma],L1][r_,s_,f2_]:>zSigmaL1f[f2],
z[\[CapitalSigma]1,L1][r_,s_,f1_]:>zSigma1L1f[f1],
\[Gamma][L1][r_]:>gammaL1,
\[Delta][B,S][r_,s_]:>deltaBS,
\[Delta][L1,\[CurlyPhi]][r_,s_]:>deltaL1Varphi,
\[Delta][W,\[CapitalXi]][r_,s_]:>deltaWXi,
\[Delta][W1,\[CapitalXi]1][r_,s_]:>deltaW1Xi1,
\[CurlyEpsilon][S,L1][r_,t_,s_]:>epsSL1,
\[CurlyEpsilon][\[CapitalXi],L1][r_,s_,t_]:>epsXiL1,
\[CurlyEpsilon][\[CapitalXi]1,L1][r_,s_,t_]:>epsXi1L1,
\[Zeta][L1,B][s_,f1_]:>zetaL1B,
\[Zeta][L1,B1][s_,f1_]:>zetaL1B1,
\[Zeta][L1,W][s_,f1_]:>zetaL1W,
\[Zeta][L1,W1][s_,f1_]:>zetaL1W1,
\[Kappa][S][r_]:>kS,
\[Kappa][\[CapitalXi]][r_]:>kXi,
\[Kappa][\[CapitalXi]1][r_]:>kXi1,
\[Kappa][S,\[CapitalXi]][t_,s_,r_]:>kSXi,
\[Kappa][S,\[CapitalXi]1][t_,r_,s_]:>kSXi1,
\[Kappa][S,\[CurlyPhi]][r_,s_]:>kSVarphi,
\[Kappa][\[CapitalXi],\[CapitalTheta]1][r_,s_]:>kXiTheta1,
\[Kappa][\[CapitalXi],\[CapitalXi]1][s_,r_,t_]:>kXiXi1,
\[Kappa][\[CapitalXi],\[CurlyPhi]][r_,s_]:>kXiVarphi,
\[Kappa][\[CapitalXi]1,\[CapitalTheta]1][r_,s_]:>kXi1Theta1,
\[Kappa][\[CapitalXi]1,\[CapitalTheta]3][r_,s_]:>kXi1Theta3,
\[Kappa][\[CapitalXi]1,\[CurlyPhi]][r_,s_]:>kXi1Varphi,
\[Kappa]3[S][r_,t_,s_]:>k3S,
\[Lambda][Df][r_,f1_]:>lamDff[f1],
\[Lambda][Ef][r_,f1_]:>lamEff[f1],
\[Lambda][Ne][r_,f1_]:>lamNef[f1],
\[Lambda][Q5][r_,f1_]:>lamQ5f[f1],
\[Lambda][Q7][r_,f1_]:>lamQ7f[f1],
\[Lambda][S][r_,s_]:>lamS,
\[Lambda]4[S]:>lam4S,
\[Lambda][T1][r_,f1_]:>lamT1f[f1],
\[Lambda][T2][r_,f1_]:>lamT2f[f1],
\[Lambda][U][r_,f1_]:>lamUf[f1],
\[Lambda][\[CapitalDelta]1][r_,f1_]:>lamDelta1f[f1],
\[Lambda][\[CapitalDelta]3][r_,f1_]:>lamDelta3f[f1],
\[Lambda][\[CapitalTheta]1][r_]:>lamTheta1,
\[Lambda][\[CapitalTheta]3][r_]:>lamTheta3,
\[Lambda][\[CapitalXi]][s_]:>lamXi,
\[Lambda][\[CapitalXi]][s_,r_]:>lamXi,
\[Lambda][\[CapitalXi]1][r_,s_]:>lamXi1,
\[Lambda][\[CapitalSigma]][r_,f1_]:>lamSigmaf[f1],
\[Lambda][\[CapitalSigma]1][r_,f1_]:>lamSigma1f[f1],
\[Lambda][\[CurlyPhi]][r_]:>lamVarphi,
\[Lambda][Df,Q1][s_,r_]:>lamDfQ1,
\[Lambda][Df,Q5][r_,s_]:>lamDfQ5,
\[Lambda][Ef,\[CapitalDelta]1][r_,s_]:>lamEfDelta1,
\[Lambda][Ef,\[CapitalDelta]3][s_,r_]:>lamEfDelta3,
\[Lambda][S,\[CapitalXi]1][r_,s_]:>lamSXi1,
\[Lambda][T1,Q1][r_,s_]:>lamT1Q1,
\[Lambda][T1,Q5][r_,s_]:>lamT1Q5,
\[Lambda][T2,Q1][s_,r_]:>lamT2Q1,
\[Lambda][T2,Q7][s_,r_]:>lamT2Q7,
\[Lambda][U,Q1][r_,s_]:>lamUQ1,
\[Lambda][U,Q7][r_,s_]:>lamUQ7,
\[Lambda][\[CapitalXi]1,\[CapitalXi]][r_,s_]:>lamXi1Xi,
\[Lambda][\[CapitalSigma],\[CapitalDelta]1][s_,r_]:>lamSigmaDelta1,
\[Lambda][\[CapitalSigma]1,\[CapitalDelta]1][r_,s_]:>lamSigma1Delta1,
\[Lambda][\[CapitalSigma]1,\[CapitalDelta]3][s_,r_]:>lamSigma1Delta3,
\[Lambda]p[\[CapitalXi]1][r_,s_]:>lampriXi1,
g[B][d][r_,f1_,f2_]:>gBdf[f1,f2],
g[B][e][r_,f1_,f2_]:>gBef[f1,f2],
g[B][L][r_,f1_,f2_]:>gBLf[f1,f2],
g[B][q][r_,f1_,f2_]:>gBqf[f1,f2],
g[B][u][r_,f1_,f2_]:>gBuf[f1,f2],
g[B][\[Phi]][r_]:>gBH,
g[B1][du][r_,f2_,f1_]:>gB1duf[f2,f1],
g[B1][\[Phi]][r_]:>gB1H,
g[G][d][r_,f1_,f2_]:>gGdf[f1,f2],
g[G][q][r_,f1_,f2_]:>gGqf[f1,f2],
g[G][u][r_,f1_,f2_]:>gGuf[f1,f2],
g[L1][B][r_,s_]:>gL1B,
g[L1][Btil][r_,s_]:>gL1Btil,
g[L1][W][s_,r_]:>gL1W,
g[L1][Wsm][s_,r_]:>gL1WSM,
g[L1][Wtil][r_,s_]:>gL1Wtil,
g[W][L][r_,f1_,f2_]:>gWLf[f1,f2],
g[W][q][r_,f1_,f2_]:>gWqf[f1,f2],
g[W][\[Phi]][f1_]:>gWH,
g[W1][\[Phi]][f1_]:>gW1H,
g[\[ScriptCapitalQ]1][dq][r_,f1_,f3_]:>gQv1dqf[f1,f3],
g[\[ScriptCapitalQ]1][uL][r_,f2_,f4_]:>gQv1uLf[f2,f4],
g[\[ScriptCapitalQ]5][dL][r_,f1_,f4_]:>gQv5dLf[f1,f4],
g[\[ScriptCapitalQ]5][eq][r_,f2_,f4_]:>gQv5eqf[f2,f4],
g[\[ScriptCapitalQ]5][uq][r_,f2_,f3_]:>gQv5uqf[f2,f3],
g[\[ScriptCapitalU]2][ed][r_,f1_,f4_]:>gUv2edf[f1,f4],
g[\[ScriptCapitalU]2][Lq][r_,f1_,f4_]:>gUv2Lqf[f1,f4],
g[\[ScriptCapitalU]5][ed][r_,f1_,f4_]:>gUv5edf[f1,f4],
y[\[Zeta]][qL][r_,f3_,f1_]:>yZetaqLf[f3,f1],
y[\[Zeta]][qq][r_,f1_,f2_]:>yZetaqqf[f1,f2],
y[\[CapitalPi]7][eq][r_,f1_,f4_]:>yPi7eqf[f1,f4],
y[\[CapitalPi]7][Lu][r_,f1_,f4_]:>yPi7Luf[f1,f4],
y[\[CapitalUpsilon]][qq][r_,f4_,f2_]:>yUpsqqf[f4,f2],
y[\[CurlyPhi]][d][r_,f1_,f2_]:>yVarphidf[f1,f2],
y[\[CurlyPhi]][e][r_,f1_,f2_]:>yVarphief[f1,f2],
y[\[CurlyPhi]][u][r_,f1_,f2_]:>yVarphiuf[f1,f2],
y[\[CapitalPhi]][dq][r_,f3_,f2_]:>yPhidqf[f3,f2],
y[\[CapitalPhi]][qu][r_,f1_,f2_]:>yPhiquf[f1,f2],
y[\[Omega]1][du][r_,f1_,f2_]:>yomega1duf[f1,f2],
y[\[Omega]1][ed][r_,f2_,f4_]:>yomega1edf[f2,f4],
y[\[Omega]1][eu][r_,f1_,f3_]:>yomega1euf[f1,f3],
y[\[Omega]1][qL][r_,f3_,f1_]:>yomega1qLf[f3,f1],
y[\[Omega]1][qq][r_,f1_,f2_]:>yomega1qqf[f1,f2],
y[\[CapitalOmega]1][qq][r_,f1_,f3_]:>yOmega1qqf[f1,f3],
y[\[CapitalOmega]1][ud][r_,f1_,f3_]:>yOmega1udf[f1,f3],
y[\[Omega]4][ed][r_,f1_,f3_]:>yomega4edf[f1,f3],
y[\[Omega]4][uu][r_,f2_,f3_]:>yomega4uuf[f2,f3],
z[Q1,L1][d][r_,s_,f1_]:>zQ1L1df[f1],
z[Q1,L1][u][s_,r_,f1_]:>zQ1L1uf[f1],
\[Lambda][Q1][d][r_,f1_]:>lamQ1df[f1],
\[Lambda][Q1][u][r_,f1_]:>lamQ1uf[f1]};
listAllUVCouplingsDummyInd=listAllUVCouplings/.{r->r_,s->s_,k->k_,t->t_,f1->f1_,f2->f2_,f3->f3_,f4->f4_};
vanishAllUVCouplings=Table[listAllUVCouplingsDummyInd[[i]]:>0,{i,1,Length[listAllUVCouplingsDummyInd]}];
tableUVCouplings=Table[If[i==j,Join[Cases[listAllUVCouplings,_[nameListAll[[i]]][__]],Cases[listAllUVCouplings,_[nameListAll[[i]]][__][__]]],Join[Cases[listAllUVCouplings,_[nameListAll[[i]],nameListAll[[j]]][__]],Cases[listAllUVCouplings,_[nameListAll[[i]],nameListAll[[j]]][__][__]],Cases[listAllUVCouplings,_[nameListAll[[j]],nameListAll[[i]]][__]],Cases[listAllUVCouplings,_[nameListAll[[j]],nameListAll[[i]]][__][__]]]],{i,1,Length[nameListAll]},{j,1,Length[nameListAll]}];
listReempAllRealAnnex=Join[Table[Re[listAllUVCouplingsDummyInd[[i]]]:>Evaluate[listAllUVCouplings[[i]]],{i,1,Length[listAllUVCouplingsDummyInd]}],Table[Re[listAllUVCouplingsDummyInd[[i]]^2]:>Evaluate[listAllUVCouplings[[i]]^2],{i,1,Length[listAllUVCouplingsDummyInd]}],
Table[Im[listAllUVCouplingsDummyInd[[i]]]:>0,{i,1,Length[listAllUVCouplingsDummyInd]}],Table[Im[listAllUVCouplingsDummyInd[[i]]^2]:>0,{i,1,Length[listAllUVCouplingsDummyInd]}]];
listReempAllReal=Join[listReempAllReal,listReempAllRealAnnex];
showCoupPart[f_]:=Block[{pos,ret,crossCoup,incPart,mmEFTdicto},pos=Position[nameListAll,f][[1,1]];
ret=tableUVCouplings[[pos,pos]];
incPart=Cases[nameListAll,_?(n[#]>0&)];
(*Print[incPart];
*)If[Length[incPart]>1,
incPart=DeleteCases[incPart,f];
crossCoup=Table[tableUVCouplings[[pos,Position[nameListAll,incPart[[i]]][[1,1]]]],{i,1,Length[incPart]}];
(*Print[crossCoup];*)
ret=Join[ret,Flatten[crossCoup]];
];
ret];
showCoupPartAll[f_]:=Block[{pos,ret,crossCoup,incPart},pos=Position[nameListAll,f][[1,1]];
ret=Flatten[tableUVCouplings[[pos]]];
ret];
allocateAllMasses:=Parallelize[ClearAll[M];massAllocator[S];massAllocator[S1];massAllocator[S2];
massAllocator[\[CurlyPhi]];massAllocator[\[CapitalXi]];massAllocator[\[CapitalXi]1];massAllocator[\[CapitalTheta]1];massAllocator[\[CapitalTheta]3];massAllocator[\[Omega]1];massAllocator[\[Omega]2];massAllocator[\[Omega]4];massAllocator[\[CapitalPi]1];massAllocator[\[CapitalPi]7];massAllocator[\[Zeta]];massAllocator[\[CapitalOmega]1];
massAllocator[\[CapitalOmega]2];
massAllocator[\[CapitalOmega]4];
massAllocator[\[CapitalUpsilon]];
massAllocator[\[CapitalPhi]];
massAllocator[B];
massAllocator[B1];
massAllocator[W];
massAllocator[W1];
massAllocator[G];
massAllocator[G1];
massAllocator[H];
massAllocator[L1];
massAllocator[L3];
massAllocator[\[ScriptCapitalU]2];
massAllocator[\[ScriptCapitalU]5];
massAllocator[\[ScriptCapitalQ]1];
massAllocator[\[ScriptCapitalQ]5];
massAllocator[X];
massAllocator[Y1];
massAllocator[Y5];
massAllocator[Ne];
massAllocator[Ef];
massAllocator[\[CapitalDelta]1];
massAllocator[\[CapitalDelta]3];
massAllocator[\[CapitalSigma]];
massAllocator[\[CapitalSigma]1];
massAllocator[U];
massAllocator[Df];
massAllocator[Y5];
massAllocator[Q1];
massAllocator[Q5];
massAllocator[Q7];
massAllocator[T1];
massAllocator[T2];];
readNameWCs[x_]:=Piecewise[{{"wwCQq18",x=="O81qq"},{"wwCQq11",x=="O11qq"},{"wwCQq38",x=="O83qq"},{"wwCQq31",x=="O13qq"},{"wwCtq8",x=="O8qt"},{"wwCtq1",x=="O1qt"},{"wwCtu8",x=="O8ut"},{"wwCtu1",x=="O1ut"},{"wwCQu8",x=="O8qu"},{"wwCQu1",x=="O1qu"},{"wwCtd8",x=="O8dt"},{"wwCtd1",x=="O1dt"},{"wwCQd8",x=="O8qd"},{"wwCQd1",x=="O1qd"},{"wwCQQ1",x=="OQQ1"},{"wwCQQ8",x=="OQQ8"},{"wwCQt1",x=="OQt1"},{"wwCQt8",x=="OQt8"},{"wwCtt1",x=="Ott1"},{"wwCll",x=="Oll"},{"wwCb\[CurlyPhi]",x=="Obp"},{"wwCt\[CurlyPhi]",x=="Otp"},{"wwCtG",x=="OtG"},{"wwCc\[CurlyPhi]",x=="Ocp"},{"wwC\[Tau]\[CurlyPhi]",x=="Otap"},{"wwCtW",x=="OtW"},{"wwCtZ",x=="OtZ"},{"wwC\[CurlyPhi]Q3",x=="O3pQ3"},{"wwC\[CurlyPhi]Qm",x=="OpQM"},{"wwC\[CurlyPhi]t",x=="Opt"},{"wwC\[CurlyPhi]l11",x=="Opl1"},{"wwC\[CurlyPhi]l31",x=="O3pl1"},{"wwC\[CurlyPhi]l12",x=="Opl2"},{"wwC\[CurlyPhi]l32",x=="O3pl2"},{"wwC\[CurlyPhi]l13",x=="Opl3"},{"wwC\[CurlyPhi]l33",x=="O3pl3"},{"wwC\[CurlyPhi]e",x=="Ope"},{"wwC\[CurlyPhi]\[Mu]",x=="Opmu"},{"wwC\[CurlyPhi]\[Tau]",x=="Opta"},{"wwC\[CurlyPhi]q3",x=="O3pq"},{"wwC\[CurlyPhi]qm",x=="OpqMi"},{"wwC\[CurlyPhi]ui",x=="Opui"},{"wwC\[CurlyPhi]di",x=="Opdi"},{"wwC\[CurlyPhi]G",x=="OpG"},{"wwC\[CurlyPhi]B",x=="OpB"},{"wwC\[CurlyPhi]W",x=="OpW"},{"wwC\[CurlyPhi]d",x=="Opd"},{"wwC\[CurlyPhi]WB",x=="OpWB"},{"wwC\[CurlyPhi]D",x=="OpD"},{"wwCWWW",x=="OWWW"}}];
mSM[\[Tau]]=Rationalize[0.00178];
mSM[\[Mu]]=0;
mSM[e]=0;
mSM[W]=Rationalize[0.080379];
mSM[Z]=Rationalize[0.0911876];
mSM[h]=Rationalize[0.12525];
mSM[t]=Rationalize[0.17276];
mSM[b]=Rationalize[0.00418];
mSM[c]=0;
mSM[u]=0;
mSM[d]=0;
mSM[s]=0;
vSM=Rationalize[0.24622];
(*////////////
Let's take diagonalised Yukawa matrices and neglect the CKM for now
///////////*)
y[e][i_,j_]:=Piecewise[{{mSM[e]*Sqrt[2]/vSM,i==1&&j==1},{mSM[\[Mu]]*Sqrt[2]/vSM,i==2&&j==2},{mSM[\[Tau]]*Sqrt[2]/vSM,i==3&&j==3}},0];
y[d][i_,j_]:=Piecewise[{{mSM[d]*Sqrt[2]/vSM,i==1&&j==1},{mSM[s]*Sqrt[2]/vSM,i==2&&j==2},{mSM[b]*Sqrt[2]/vSM,i==3&&j==3}},0];
y[u][i_,j_]:=Piecewise[{{mSM[u]*Sqrt[2]/vSM,i==1&&j==1},{mSM[c]*Sqrt[2]/vSM,i==2&&j==2},{mSM[t]*Sqrt[2]/vSM,i==3&&j==3}},0];
gHat[B][\[Phi]][r_]:=g[B][\[Phi]][r]-I*Sum[Conjugate[\[Zeta][L1,B][s,r]]\[Gamma][L1][s]/(M[L1][s]^2),{s,1,n[L1]}];
gHat[W][\[Phi]][r_]:=g[W][\[Phi]][r]-2I*Sum[Conjugate[\[Zeta][L1,W][s,r]]\[Gamma][L1][s]/(M[L1][s]^2),{s,1,n[L1]}];
gHat[B1][\[Phi]][r_]:=g[B1][\[Phi]][r]+I*Sum[Conjugate[\[Zeta][L1,B1][s,r]]\[Gamma][L1][s]/(M[L1][s]^2),{s,1,n[L1]}];
gHat[W1][\[Phi]][r_]:=g[W1][\[Phi]][r]+2I*Sum[Conjugate[\[Zeta][L1,W1][s,r]]\[Gamma][L1][s]/(M[L1][s]^2),{s,1,n[L1]}];
Z\[Phi]:=1-Sum[(Conjugate[\[Gamma][L1][r]] \[Gamma][L1][r])/(M[L1][r]^2),{r,1,n[L1]}]-\[Mu]Hat[\[Phi]]^2*If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]Conjugate[\[Gamma][L1][r]]\[Delta][L1,\[CurlyPhi]][r,s]\[Gamma][L1][t],{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}],0];
(*////The following definitions constitute a system of equations. I use below the solution to such system////*)
fAux1[f_][i_,j_]:=Piecewise[{{1,ToString[f]=="u"},{-1,ToString[f]=="d"},{-1,ToString[f]=="e"}},1]*If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[\[Delta][L1,\[CurlyPhi]][s,r]Conjugate[\[Gamma][L1][s]]Piecewise[{{Conjugate[y[\[CurlyPhi]][u][r,i,j]],ToString[f]=="u"}},y[\[CurlyPhi]][f][r,i,j]]/(M[\[CurlyPhi]][r]^2M[L1][s]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[L1]}],0];
fAux2[f_][i_,j_]:=If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]\[Gamma][L1][t]\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]/(2M[L1][r]^2M[\[CurlyPhi]][s]^2M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}],0];
fAux3:=-2(If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[(Conjugate[\[Gamma][L1][r]]* Conjugate[\[Delta][L1, \[CurlyPhi]][t, s]]\[Gamma][L1][t]*\[Delta][L1, \[CurlyPhi]][r, s])/(M[L1][r]^2*M[L1][t]^2 * M[\[CurlyPhi]][s]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}],0])\[Mu]Hat[\[Phi]]^2;
fAux4:=Sum[\[Kappa][S][r]^2/(2*M[S][r]^2),{r,1,n[S]}]+Sum[\[Kappa][\[CapitalXi]][r]^2/(2*M[\[CapitalXi]][r]^2),{r,1,n[\[CapitalXi]]}]+2*Sum[(Conjugate[\[Kappa][\[CapitalXi]1][r]]*\[Kappa][\[CapitalXi]1][r])/M[\[CapitalXi]1][r]^2,{r,1,n[\[CapitalXi]1]}]-2*Sum[\[Kappa][\[CapitalXi]][r]^2/M[\[CapitalXi]][r]^4,{r,1,n[\[CapitalXi]]}]*\[Mu]Hat[\[Phi]]^2-4*Sum[(Conjugate[\[Kappa][\[CapitalXi]1][r]]*\[Kappa][\[CapitalXi]1][r])/M[\[CapitalXi]1][r]^4,{r,1,n[\[CapitalXi]1]}]*\[Mu]Hat[\[Phi]]^2+Sum[(Conjugate[gHat[B1][\[Phi]][r]]*gHat[B1][\[Phi]][r])/M[B1][r]^2,{r,1,n[B1]}]*\[Mu]Hat[\[Phi]]^2+Sum[(Conjugate[gHat[W][\[Phi]][r]]*gHat[W][\[Phi]][r])/(2*M[W][r]^2),{r,1,n[W]}]*\[Mu]Hat[\[Phi]]^2+Sum[(Conjugate[gHat[W1][\[Phi]][r]]*gHat[W1][\[Phi]][r])/(4*M[W1][r]^2),{r,1,n[W1]}]*\[Mu]Hat[\[Phi]]^2-4*If[n[L1]>0&&n[\[CapitalXi]]>0,Sum[Re[Conjugate[g1[\[CapitalXi]1,L1][r,s]]*Conjugate[\[Gamma][L1][s]]*\[Kappa][\[CapitalXi]][r]]/(M[L1][s]^2*M[\[CapitalXi]1][r]^2),{r,1,n[\[CapitalXi]]},{s,1,n[L1]}],0]*\[Mu]Hat[\[Phi]]^2-2*If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[Re[Conjugate[\[Gamma][L1][r]]*\[Delta][L1,\[CurlyPhi]][r,s]*\[Lambda][\[CurlyPhi]][s]]/(M[L1][r]^2*M[\[CurlyPhi]][s]^2),{s,1,n[\[CurlyPhi]]},{r,1,n[L1]}],0]*\[Mu]Hat[\[Phi]]^2+Sum[(Conjugate[\[Gamma][L1][r]]*h1[L1][r,s]*\[Gamma][L1][s])/(M[L1][r]^2*M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]*\[Mu]Hat[\[Phi]]^2-2*Sum[(Re[g2[S,L1][r,s]*\[Gamma][L1][s]]*\[Kappa][S][r])/(M[L1][s]^2*M[S][r]^2),{r,1,n[S]},{s,1,n[L1]}]*\[Mu]Hat[\[Phi]]^2-4*If[n[L1]>0&&n[\[CapitalXi]]>0,Sum[(Re[Conjugate[g1[\[CapitalXi],L1][r,s]]*Conjugate[\[Gamma][L1][s]]]*\[Kappa][\[CapitalXi]][r])/(M[L1][s]^2*M[\[CapitalXi]][r]^2),{r,1,n[\[CapitalXi]]},{s,1,n[L1]}],0]*\[Mu]Hat[\[Phi]]^2+2*If[n[L1]>0&&n[\[CapitalXi]]>0,Sum[(Re[g2[\[CapitalXi],L1][r,s]*\[Gamma][L1][s]]*\[Kappa][\[CapitalXi]][r])/(M[L1][s]^2*M[\[CapitalXi]][r]^2),{r,1,n[\[CapitalXi]]},{s,1,n[L1]}],0]*\[Mu]Hat[\[Phi]]^2+2*Sum[(Im[gHat[W][\[Phi]][r]]*\[Delta][W,\[CapitalXi]][r,s]*\[Kappa][\[CapitalXi]][s])/(M[W][r]^2*M[\[CapitalXi]][s]^2),{r,1,n[W]},{s,1,n[\[CapitalXi]]}]*\[Mu]Hat[\[Phi]]^2+2*If[n[W1]>0&&n[\[CapitalXi]1]>0,Sum[(Im[gHat[W1][\[Phi]][r]]*\[Delta][W1,\[CapitalXi]1][r,s]*\[Kappa][\[CapitalXi]1][s])/(M[W1][r]^2*M[\[CapitalXi]1][s]^2),{r,1,n[W1]},{s,1,n[\[CapitalXi]1]}],0]*\[Mu]Hat[\[Phi]]^2-Subscript[g2,SM]*Sum[(Conjugate[\[Gamma][L1][s]]*\[Gamma][L1][r]*g[L1][Wsm][s,r])/(M[L1][r]^2*M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]*\[Mu]Hat[\[Phi]]^2+4*If[n[L1]>0&&n[\[CurlyPhi]]>0&&n[\[CapitalXi]1]>0,Sum[Re[Conjugate[\[Gamma][L1][r]]*Conjugate[\[Kappa][\[CapitalXi]1,\[CurlyPhi]][t,s]]*\[Delta][L1,\[CurlyPhi]][r,s]*\[Kappa][\[CapitalXi]1][t]]/(M[L1][r]^2*M[\[CapitalXi]1][t]^2*M[\[CurlyPhi]][s]^2),{s,1,n[\[CurlyPhi]]},{t,1,n[\[CapitalXi]1]},{r,1,n[L1]}],0]*\[Mu]Hat[\[Phi]]^2-If[n[L1]>0&&n[S]>0,Sum[(Conjugate[\[Gamma][L1][t]]*\[Gamma][L1][s]*\[CurlyEpsilon][S,L1][r,t,s]*\[Kappa][S][r])/(M[L1][s]^2*M[L1][t]^2*M[S][r]^2),{r,1,n[S]},{s,1,n[L1]},{t,1,n[L1]}],0]*\[Mu]Hat[\[Phi]]^2+2*If[n[L1]>0&&n[S]>0&&n[\[CurlyPhi]]>0,Sum[(Re[Conjugate[\[Gamma][L1][r]]*\[Delta][L1,\[CurlyPhi]][r,s]*\[Kappa][S,\[CurlyPhi]][t,s]]*\[Kappa][S][t])/(M[L1][r]^2*M[S][t]^2*M[\[CurlyPhi]][s]^2),{s,1,n[\[CurlyPhi]]},{t,1,n[S]},{r,1,n[L1]}],0]*\[Mu]Hat[\[Phi]]^2+If[n[L1]>0&&n[\[CapitalXi]]>0,Sum[(Conjugate[\[Gamma][L1][r]]*\[Gamma][L1][t]*\[CurlyEpsilon][\[CapitalXi],L1][s,r,t]*\[Kappa][\[CapitalXi]][s])/(M[L1][r]^2*M[L1][t]^2*M[\[CapitalXi]][s]^2),{r,1,n[L1]},{s,1,n[\[CapitalXi]]},{t,1,n[L1]}],0]*\[Mu]Hat[\[Phi]]^2+2*If[n[W]>0&&n[\[CapitalXi]]>0,Sum[(\[Delta][W,\[CapitalXi]][t,r]*\[Delta][W,\[CapitalXi]][t,s]*\[Kappa][\[CapitalXi]][r]*\[Kappa][\[CapitalXi]][s])/(M[W][t]^2*M[\[CapitalXi]][r]^2*M[\[CapitalXi]][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[\[CapitalXi]]},{t,1,n[W]}],0]*\[Mu]Hat[\[Phi]]^2+2*If[n[L1]>0&&n[\[CapitalXi]]>0&&n[\[CurlyPhi]]>0,Sum[(Re[Conjugate[\[Gamma][L1][r]]*\[Delta][L1,\[CurlyPhi]][r,s]*\[Kappa][\[CapitalXi],\[CurlyPhi]][t,s]]*\[Kappa][\[CapitalXi]][t])/(M[L1][r]^2*M[\[CapitalXi]][t]^2*M[\[CurlyPhi]][s]^2),{s,1,n[\[CurlyPhi]]},{t,1,n[\[CapitalXi]]},{r,1,n[L1]}],0]*\[Mu]Hat[\[Phi]]^2+4*If[n[W1]>0&&n[\[CapitalXi]1]>0,Sum[(Conjugate[\[Delta][W1,\[CapitalXi]1][s,t]]*Conjugate[\[Kappa][\[CapitalXi]1][t]]*\[Delta][W1,\[CapitalXi]1][s,r]*\[Kappa][\[CapitalXi]1][r])/(M[W1][s]^2*M[\[CapitalXi]1][r]^2*M[\[CapitalXi]1][t]^2),{r,1,n[\[CapitalXi]1]},{t,1,n[\[CapitalXi]1]},{s,1,n[W1]}],0]*\[Mu]Hat[\[Phi]]^2;
fAux5:=1-Sum[(Conjugate[\[Gamma][L1][r]] \[Gamma][L1][r])/(M[L1][r]^2),{r,1,n[L1]}];
fAux6:=-If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]Conjugate[\[Gamma][L1][r]]\[Delta][L1,\[CurlyPhi]][r,s]*\[Gamma][L1][t],{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}],0];
yHat[f_][i_,j_]:=(Sqrt[Z\[Phi]]ySM[f][i,j]-\[Mu]Hat[\[Phi]]^2fAux1[f][i,j])/(1-\[Mu]Hat[\[Phi]]^2fAux2[f][i,j]);
\[Lambda]Hat[\[Phi]]:=(Z\[Phi]^2\[Lambda]\[Phi]-fAux4)/(1+fAux3);
\[Mu]Hat[\[Phi]]:=Sqrt[(fAux5*Subscript[\[Mu], \[Phi]]^2)/(1-fAux6*Subscript[\[Mu], \[Phi]]^2)];
(*//// General expressions for all the WCs after tree-level matching. ////*)
Subscript[wC, y][e][i_,j_]:=(-\[Mu]Hat[\[Phi]]^2If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[\[Delta][L1,\[CurlyPhi]][s,r]Conjugate[\[Gamma][L1][s]]y[\[CurlyPhi]][e][r,i,j]/(M[\[CurlyPhi]][r]^2M[L1][s]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[L1]}],0]-\[Mu]Hat[\[Phi]]^2yHat[e][i,j]If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]\[Gamma][L1][t]\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]/(2M[L1][r]^2M[\[CurlyPhi]][s]^2M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}],0])*(Z\[Phi])^(-1/2);
Subscript[wC, y][d][i_,j_]:=(-\[Mu]Hat[\[Phi]]^2If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[\[Delta][L1,\[CurlyPhi]][s,r]Conjugate[\[Gamma][L1][s]]y[\[CurlyPhi]][d][r,i,j]/(M[\[CurlyPhi]][r]^2M[L1][s]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[L1]}],0]-\[Mu]Hat[\[Phi]]^2yHat[d][i,j]If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]\[Gamma][L1][t]\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]/(2M[L1][r]^2M[\[CurlyPhi]][s]^2M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}],0])*(Z\[Phi])^(-1/2);
Subscript[wC, y][u][i_,j_]:=(-\[Mu]Hat[\[Phi]]^2If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[\[Delta][L1,\[CurlyPhi]][s,r]Conjugate[\[Gamma][L1][s]]Conjugate[y[\[CurlyPhi]][u][r,i,j]]/(M[\[CurlyPhi]][r]^2M[L1][s]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[L1]}],0]-\[Mu]Hat[\[Phi]]^2If[n[L1]>0&&n[\[CurlyPhi]]>0,yHat[u][i,j]Sum[Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]\[Gamma][L1][t]\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]/(2M[L1][r]^2M[\[CurlyPhi]][s]^2M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}],0])*(Z\[Phi])^(-1/2);
Subscript[wC, \[Phi]4]:=(Sum[\[Kappa][S][r]^2/(2M[S][r]^2),{r,1,n[S]}]+Sum[\[Kappa][\[CapitalXi]][r]^2/(2M[\[CapitalXi]][r]^2),{r,1,n[\[CapitalXi]]}]-2\[Mu]Hat[\[Phi]]^2Sum[\[Kappa][\[CapitalXi]][r]^2/(M[\[CapitalXi]][r]^4),{r,1,n[\[CapitalXi]]}]+2Sum[Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Kappa][\[CapitalXi]1][r]/(M[\[CapitalXi]1][r]^2),{r,1,n[\[CapitalXi]1]}]-4\[Mu]Hat[\[Phi]]^2Sum[Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Kappa][\[CapitalXi]1][r]/(M[\[CapitalXi]1][r]^4),{r,1,n[\[CapitalXi]1]}]+\[Mu]Hat[\[Phi]]^2Sum[Conjugate[gHat[B1][\[Phi]][r]]gHat[B1][\[Phi]][r]/(M[B1][r]^2),{r,1,n[B1]}]+\[Mu]Hat[\[Phi]]^2Sum[Conjugate[gHat[W][\[Phi]][r]]gHat[W][\[Phi]][r]/(2M[W][r]^2),{r,1,n[W]}]+\[Mu]Hat[\[Phi]]^2Sum[Conjugate[gHat[W1][\[Phi]][r]]gHat[W1][\[Phi]][r]/(4M[W1][r]^2),{r,1,n[W1]}]-\[Mu]Hat[\[Phi]]^2 Subscript[g2, SM] Sum[Conjugate[\[Gamma][L1][s]]\[Gamma][L1][r]g[L1][Wsm][s,r]/(M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]+\[Mu]Hat[\[Phi]]^2Sum[Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]h1[L1][r,s]/(M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]+2\[Mu]Hat[\[Phi]]^2If[n[\[CapitalXi]]>0&&n[W]>0,Sum[Im[gHat[W][\[Phi]][r]]\[Delta][W,\[CapitalXi]][r,s]\[Kappa][\[CapitalXi]][s]/(M[W][r]^2M[\[CapitalXi]][s]^2),{r,1,n[W]},{s,1,n[\[CapitalXi]]}],0]+2\[Mu]Hat[\[Phi]]^2If[n[\[CapitalXi]]>0&&n[W]>0,Sum[\[Delta][W,\[CapitalXi]][t,s]\[Delta][W,\[CapitalXi]][t,r]\[Kappa][\[CapitalXi]][r]\[Kappa][\[CapitalXi]][s]/(M[\[CapitalXi]][r]^2M[\[CapitalXi]][s]^2M[W][t]^2),{r,1,n[\[CapitalXi]]},{s,1,n[\[CapitalXi]]},{t,1,n[W]}],0]+2\[Mu]Hat[\[Phi]]^2If[n[\[CapitalXi]1]>0&&n[W1]>0,Sum[Im[gHat[W1][\[Phi]][r]]\[Delta][W1,\[CapitalXi]1][r,s]\[Kappa][\[CapitalXi]1][s]/(M[W1][r]^2M[\[CapitalXi]1][s]^2),{r,1,n[W1]},{s,1,n[\[CapitalXi]1]}],0]+4\[Mu]Hat[\[Phi]]^2If[n[\[CapitalXi]1]>0&&n[W1]>0,Sum[Conjugate[\[Delta][W1,\[CapitalXi]1][s,t]]\[Delta][W1,\[CapitalXi]1][s,r]\[Kappa][\[CapitalXi]1][r]Conjugate[\[Kappa][\[CapitalXi]1][t]]/(M[\[CapitalXi]1][r]^2M[\[CapitalXi]1][t]^2M[W1][s]^2),{r,1,n[\[CapitalXi]1]},{t,1,n[\[CapitalXi]1]},{s,1,n[W1]}],0]-2\[Mu]Hat[\[Phi]]^2If[n[L1]>0&&n[S]>0,Sum[Re[g2[S,L1][r,s]\[Gamma][L1][s]]\[Kappa][S][r]/(M[S][r]^2M[L1][s]^2),{r,1,n[S]},{s,1,n[L1]}],0]-\[Mu]Hat[\[Phi]]^2If[n[L1]>0&&n[S]>0,Sum[\[CurlyEpsilon][S,L1][r,t,s]\[Kappa][S][r]Conjugate[\[Gamma][L1][t]]\[Gamma][L1][s]/(M[S][r]^2M[L1][s]^2M[L1][t]^2),{r,1,n[S]},{s,1,n[L1]},{t,1,n[L1]}],0]-2\[Mu]Hat[\[Phi]]^2If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[Re[\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]\[Lambda][\[CurlyPhi]][s]]/(M[\[CurlyPhi]][s]^2M[L1][r]^2),{s,1,n[\[CurlyPhi]]},{r,1,n[L1]}],0]-2\[Mu]Hat[\[Phi]]^2If[n[L1]>0&&n[\[CurlyPhi]]>0,\[Lambda]Hat[\[Phi]]Sum[Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]\[Gamma][L1][t]\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]/(M[L1][r]^2M[\[CurlyPhi]][s]^2M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}],0]+2\[Mu]Hat[\[Phi]]^2If[n[L1]>0&&n[\[CapitalXi]]>0,Sum[Re[g2[\[CapitalXi],L1][r,s]\[Gamma][L1][s]]\[Kappa][\[CapitalXi]][r]/(M[\[CapitalXi]][r]^2M[L1][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[L1]}],0]+\[Mu]Hat[\[Phi]]^2If[n[L1]>0&&n[\[CapitalXi]]>0,Sum[\[CurlyEpsilon][\[CapitalXi],L1][s,r,t]\[Kappa][\[CapitalXi]][s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][t]/(M[L1][r]^2M[\[CapitalXi]][s]^2M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CapitalXi]]},{t,1,n[L1]}],0]-4\[Mu]Hat[\[Phi]]^2If[n[L1]>0&&n[\[CapitalXi]]>0,Sum[Re[Conjugate[g1[\[CapitalXi],L1][r,s]]Conjugate[\[Gamma][L1][s]]]\[Kappa][\[CapitalXi]][r]/(M[\[CapitalXi]][r]^2M[L1][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[L1]}],0]-4\[Mu]Hat[\[Phi]]^2If[n[L1]>0&&n[\[CapitalXi]1]>0,Sum[Re[Conjugate[g1[\[CapitalXi]1,L1][r,s]]Conjugate[\[Gamma][L1][s]]\[Kappa][\[CapitalXi]1][r]]/(M[\[CapitalXi]1][r]^2M[L1][s]^2),{r,1,n[\[CapitalXi]1]},{s,1,n[L1]}],0]+2\[Mu]Hat[\[Phi]]^2If[n[L1]>0&&n[\[CurlyPhi]]>0&&n[S]>0,Sum[Re[\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]\[Kappa][S,\[CurlyPhi]][t,s]]\[Kappa][S][t]/(M[\[CurlyPhi]][s]^2M[S][t]^2M[L1][r]^2),{s,1,n[\[CurlyPhi]]},{t,1,n[S]},{r,1,n[L1]}],0]+2\[Mu]Hat[\[Phi]]^2If[n[L1]>0&&n[\[CurlyPhi]]>0&&n[\[CapitalXi]]>0,Sum[Re[\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]\[Kappa][\[CapitalXi],\[CurlyPhi]][t,s]]\[Kappa][\[CapitalXi]][t]/(M[\[CurlyPhi]][s]^2M[\[CapitalXi]][t]^2M[L1][r]^2),{s,1,n[\[CurlyPhi]]},{t,1,n[\[CapitalXi]]},{r,1,n[L1]}],0]+4\[Mu]Hat[\[Phi]]^2If[n[L1]>0&&n[\[CurlyPhi]]>0&&n[\[CapitalXi]1]>0,Sum[Re[\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]Conjugate[\[Kappa][\[CapitalXi]1,\[CurlyPhi]][t,s]]\[Kappa][\[CapitalXi]1][t]]/(M[\[CurlyPhi]][s]^2M[\[CapitalXi]1][t]^2M[L1][r]^2),{s,1,n[\[CurlyPhi]]},{t,1,n[\[CapitalXi]1]},{r,1,n[L1]}],0])*(Z\[Phi])^(-2);
Subscript[wC, 5][i_,j_]:=(-2Sum[(\[Kappa][\[CapitalXi]1][r])(Conjugate[y[\[CapitalXi]1][r,j,i]])/(M[\[CapitalXi]1][r]^2),{r,1,n[\[CapitalXi]1]}]+Sum[(\[Lambda][Ne][r,j])(\[Lambda][Ne][r,i])/(2M[Ne][r]),{r,1,n[Ne]}]+Sum[(\[Lambda][\[CapitalSigma]][r,j])(\[Lambda][\[CapitalSigma]][r,i])/(8M[\[CapitalSigma]][r]),{r,1,n[\[CapitalSigma]]}])/Z\[Phi];
Subscript[wC, ll][i_,j_,k_,l_]:=Sum[(Conjugate[y[S1][r,j,l]])(y[S1][r,i,k])/(M[S1][r]^2),{r,1,n[S1]}]+Sum[(Conjugate[y[\[CapitalXi]1][r,l,j]])(y[\[CapitalXi]1][r,l,j])/(M[\[CapitalXi]1][r]^2),{r,1,n[\[CapitalXi]1]}]-Sum[g[B][L][r,k,l]*g[B][L][r,i,j]/(2 M[B][r]^2),{r,1,n[B]}]-Sum[g[W][L][r,k,j]*g[W][L][r,i,l]/(4 M[W][r]^2),{r,1,n[W]}]+Sum[g[W][L][r,k,l]*g[W][L][r,i,j]/(8 M[W][r]^2),{r,1,n[W]}];
Subscript[wC, qq1][i_,j_,k_,l_]:=Sum[(Conjugate[y[\[Omega]1][qq][r,l,j]])(y[\[Omega]1][qq][r,i,k])/(2M[\[Omega]1][r]^2),{r,1,n[\[Omega]1]}]+3Sum[(Conjugate[y[\[Zeta]][qq][r,l,j]])(y[\[Zeta]][qq][r,k,i])/(2M[\[Zeta]][r]^2),{r,1,n[\[Zeta]]}]+Sum[(Conjugate[y[\[CapitalOmega]1][qq][r,i,k]])(y[\[CapitalOmega]1][qq][r,j,l])/(4M[\[CapitalOmega]1][r]^2),{r,1,n[\[CapitalOmega]1]}]+3Sum[(Conjugate[y[\[CapitalUpsilon]][r,k,i]])(y[\[CapitalUpsilon]][r,l,j])/(4M[\[CapitalUpsilon]][r]^2),{r,1,n[\[CapitalUpsilon]]}]-Sum[g[B][q][r,k,l]g[B][q][r,i,j]/(2 M[B][r]^2),{r,1,n[B]}]-Sum[g[G][q][r,k,j]g[G][q][r,i,l]/(8 M[G][r]^2),{r,1,n[G]}]+Sum[g[G][q][r,k,l]g[G][q][r,i,j]/(12 M[G][r]^2),{r,1,n[G]}]-3Sum[g[H][r,k,j]g[H][r,i,l]/(32 M[H][r]^2),{r,1,n[H]}];
Subscript[wC, qq3][i_,j_,k_,l_]:=-Sum[(Conjugate[y[\[Omega]1][qq][r,j,l]])(y[\[Omega]1][qq][r,k,i])/(2M[\[Omega]1][r]^2),{r,1,n[\[Omega]1]}]-Sum[(Conjugate[y[\[Zeta]][qq][r,j,l]])(y[\[Zeta]][qq][r,k,i])/(2M[\[Zeta]][r]^2),{r,1,n[\[Zeta]]}]+Sum[(Conjugate[y[\[CapitalOmega]1][qq][r,k,i]])(y[\[CapitalOmega]1][qq][r,l,j])/(4M[\[CapitalOmega]1][r]^2),{r,1,n[\[CapitalOmega]1]}]+Sum[(Conjugate[y[\[CapitalUpsilon]][r,k,i]])(y[\[CapitalUpsilon]][r,j,l])/(4M[\[CapitalUpsilon]][r]^2),{r,1,n[\[CapitalUpsilon]]}]-Sum[g[W][q][r,k,l]g[W][q][r,i,j]/(8 M[W][r]^2),{r,1,n[W]}]-Sum[g[G][q][r,k,j]g[G][q][r,i,l]/(8 M[G][r]^2),{r,1,n[G]}]+Sum[g[H][r,k,l]g[H][r,i,j]/(48 M[H][r]^2),{r,1,n[H]}]+Sum[g[H][r,k,j]g[H][r,i,l]/(32 M[H][r]^2),{r,1,n[H]}];
Subscript[wC, lq1][i_,j_,k_,l_]:=Sum[(Conjugate[y[\[Omega]1][qL][r,k,i]])(y[\[Omega]1][qL][r,l,j])/(4M[\[Omega]1][r]^2),{r,1,n[\[Omega]1]}]+3Sum[(Conjugate[y[\[Zeta]][qL][r,k,i]])(y[\[Zeta]][qL][r,l,j])/(4M[\[Zeta]][r]^2),{r,1,n[\[Zeta]]}]-Sum[g[B][q][r,k,l]g[B][L][r,i,j]/( M[B][r]^2),{r,1,n[B]}]-Sum[Conjugate[g[\[ScriptCapitalU]2][Lq][r,j,k]]g[\[ScriptCapitalU]2][Lq][r,i,l]/(2 M[\[ScriptCapitalU]2][r]^2),{r,1,n[\[ScriptCapitalU]2]}]-3Sum[Conjugate[g[X][r,j,k]]g[X][r,i,l]/(8 M[X][r]^2),{r,1,n[X]}];
Subscript[wC, lq3][i_,j_,k_,l_]:=-Sum[(Conjugate[y[\[Omega]1][qL][r,k,i]])(y[\[Omega]1][qL][r,l,j])/(4M[\[Omega]1][r]^2),{r,1,n[\[Omega]1]}]+Sum[(Conjugate[y[\[Zeta]][qL][r,k,i]])(y[\[Zeta]][qL][r,l,j])/(4M[\[Zeta]][r]^2),{r,1,n[\[Zeta]]}]-Sum[g[W][q][r,k,l]g[W][L][r,i,j]/( M[W][r]^2),{r,1,n[W]}]-Sum[Conjugate[g[\[ScriptCapitalU]2][Lq][r,j,k]]g[\[ScriptCapitalU]2][Lq][r,i,l]/(2 M[\[ScriptCapitalU]2][r]^2),{r,1,n[\[ScriptCapitalU]2]}]+Sum[Conjugate[g[X][r,j,k]]g[X][r,i,l]/(8 M[X][r]^2),{r,1,n[X]}];
Subscript[wC, ee][i_,j_,k_,l_]:=Sum[(Conjugate[y[S2][r,l,j]])(y[S2][r,k,i])/(2M[S2][r]^2),{r,1,n[S2]}]-Sum[g[B][e][r,k,l]g[B][e][r,i,j]/(2 M[B][r]^2),{r,1,n[B]}];
Subscript[wC, dd][i_,j_,k_,l_]:=Sum[(Conjugate[y[\[Omega]2][r,l,j]])(y[\[Omega]2][r,k,i])/(M[\[Omega]2][r]^2),{r,1,n[\[Omega]2]}]+Sum[(Conjugate[y[\[CapitalOmega]2][r,i,k]])(y[\[CapitalOmega]2][r,j,l])/(2M[\[CapitalOmega]2][r]^2),{r,1,n[\[CapitalOmega]2]}]-Sum[g[B][d][r,k,l]g[B][d][r,i,j]/(2 M[B][r]^2),{r,1,n[B]}]-Sum[g[G][d][r,k,j]g[G][d][r,i,l]/(4 M[G][r]^2),{r,1,n[G]}]+Sum[g[G][d][r,k,l]g[G][d][r,i,j]/(12 M[G][r]^2),{r,1,n[G]}];
Subscript[wC, uu][i_,j_,k_,l_]:=Sum[(Conjugate[y[\[Omega]4][uu][r,l,j]])(y[\[Omega]4][uu][r,k,i])/(M[\[Omega]4][r]^2),{r,1,n[\[Omega]4]}]+Sum[(Conjugate[y[\[CapitalOmega]4][r,i,k]])(y[\[CapitalOmega]4][r,j,l])/(2M[\[CapitalOmega]4][r]^2),{r,1,n[\[CapitalOmega]4]}]-Sum[g[B][u][r,k,l]g[B][u][r,i,j]/(2 M[B][r]^2),{r,1,n[B]}]-Sum[g[G][u][r,k,j]g[G][u][r,i,l]/(4 M[G][r]^2),{r,1,n[G]}]+Sum[g[G][u][r,k,l]g[G][u][r,i,j]/(12 M[G][r]^2),{r,1,n[G]}];
Subscript[wC, ed][i_,j_,k_,l_]:=Sum[(Conjugate[y[\[Omega]4][ed][r,i,k]])(y[\[Omega]4][ed][r,j,l])/(2M[\[Omega]4][r]^2),{r,1,n[\[Omega]4]}]-Sum[g[B][d][r,k,l]g[B][e][r,i,j]/(M[B][r]^2),{r,1,n[B]}]-Sum[Conjugate[g[\[ScriptCapitalU]2][ed][r,j,k]]g[\[ScriptCapitalU]2][ed][r,i,l]/( M[\[ScriptCapitalU]2][r]^2),{r,1,n[\[ScriptCapitalU]2]}];
Subscript[wC, eu][i_,j_,k_,l_]:=Sum[(Conjugate[y[\[Omega]1][eu][r,i,k]])(y[\[Omega]1][ed][r,j,l])/(2M[\[Omega]1][r]^2),{r,1,n[\[Omega]1]}]-Sum[g[B][u][r,k,l]g[B][e][r,i,j]/(M[B][r]^2),{r,1,n[B]}]-Sum[Conjugate[g[\[ScriptCapitalU]5][ed][r,j,k]]g[\[ScriptCapitalU]5][ed][r,i,l]/( M[\[ScriptCapitalU]5][r]^2),{r,1,n[\[ScriptCapitalU]5]}];
Subscript[wC, ud1][i_,j_,k_,l_]:=Sum[(Conjugate[y[\[Omega]1][du][r,l,j]])(y[\[Omega]1][du][r,k,i])/(3M[\[Omega]1][r]^2),{r,1,n[\[Omega]1]}]+Sum[(Conjugate[y[\[CapitalOmega]1][ud][r,i,k]])(y[\[CapitalOmega]1][ud][r,j,l])/(3M[\[CapitalOmega]1][r]^2),{r,1,n[\[CapitalOmega]1]}]-Sum[g[B][d][r,k,l]g[B][u][r,i,j]/(M[B][r]^2),{r,1,n[B]}]-Sum[Conjugate[g[B1][du][r,l,i]]g[B1][du][r,k,j]/(3M[B1][r]^2),{r,1,n[B1]}]-4Sum[Conjugate[g[G1][r,l,i]]g[G1][r,k,j]/(9M[G1][r]^2),{r,1,n[G1]}];
Subscript[wC, ud8][i_,j_,k_,l_]:=-Sum[(Conjugate[y[\[Omega]1][du][r,l,j]])(y[\[Omega]1][du][r,k,i])/(M[\[Omega]1][r]^2),{r,1,n[\[Omega]1]}]+Sum[(Conjugate[y[\[CapitalOmega]1][ud][r,i,k]])(y[\[CapitalOmega]1][ud][r,j,l])/(2M[\[CapitalOmega]1][r]^2),{r,1,n[\[CapitalOmega]1]}]-Sum[g[G][d][r,k,l]g[G][u][r,i,j]/(M[G][r]^2),{r,1,n[G]}]-2Sum[Conjugate[g[B1][du][r,l,i]]g[B1][du][r,k,j]/(M[B1][r]^2),{r,1,n[B1]}]+Sum[Conjugate[g[G1][r,l,i]]g[G1][r,k,j]/(3M[G1][r]^2),{r,1,n[G1]}];
Subscript[wC, le][i_,j_,k_,l_]:=-Sum[(Conjugate[y[\[CurlyPhi]][e][r,l,i]])(y[\[CurlyPhi]][e][r,k,j])/(2M[\[CurlyPhi]][r]^2),{r,1,n[\[CurlyPhi]]}]-Sum[g[B][e][r,k,l]g[B][e][r,i,j]/(M[B][r]^2),{r,1,n[B]}]+Sum[Conjugate[g[L3][r,k,i]](g[L3][r,l,j])/(M[L3][r]^2),{r,1,n[L3]}]-If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[Conjugate[yHat[e][l,i]]\[Delta][L1,\[CurlyPhi]][s,r]Conjugate[\[Gamma][L1][s]](y[\[CurlyPhi]][e][r,k,j])/(2M[\[CurlyPhi]][r]^2 *M[L1][s]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[L1]}],0]-If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[yHat[e][k,j]*Conjugate[\[Delta][L1,\[CurlyPhi]][s,r]]*\[Gamma][L1][s]*Conjugate[(y[\[CurlyPhi]][e][r,l,i])]/(2M[\[CurlyPhi]][r]^2 *M[L1][s]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[L1]}],0]-If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[yHat[e][k,j]*Conjugate[yHat[e][l,i]]*Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]* \[Gamma][L1][t]*\[Delta][L1,\[CurlyPhi]][r,s]*Conjugate[\[Gamma][L1][r]]/(2M[\[CurlyPhi]][s]^2 *M[L1][r]^2*M[L1][t]^2),{s,1,n[\[CurlyPhi]]},{r,1,n[L1]},{t,1,n[L1]}],0];
Subscript[wC, ld][i_,j_,k_,l_]:=-Sum[(Conjugate[y[\[CapitalPi]1][r,j,k]])(y[\[CapitalPi]1][r,i,l])/(2M[\[CapitalPi]1][r]^2),{r,1,n[\[CapitalPi]1]}]-Sum[g[B][d][r,k,l]g[B][L][r,i,j]/(M[B][r]^2),{r,1,n[B]}]+Sum[Conjugate[g[\[ScriptCapitalQ]5][dL][r,k,i]](g[\[ScriptCapitalQ]5][dL][r,l,j])/(M[\[ScriptCapitalQ]5][r]^2),{r,1,n[\[ScriptCapitalQ]5]}];
Subscript[wC, lu][i_,j_,k_,l_]:=-Sum[(Conjugate[y[\[CapitalPi]7][Lu][r,j,k]])(y[\[CapitalPi]7][Lu][r,i,l])/(2M[\[CapitalPi]7][r]^2),{r,1,n[\[CapitalPi]7]}]-Sum[g[B][u][r,k,l]g[B][L][r,i,j]/(M[B][r]^2),{r,1,n[B]}]+Sum[Conjugate[g[\[ScriptCapitalQ]1][uL][r,k,i]](g[\[ScriptCapitalQ]1][uL][r,l,j])/(M[\[ScriptCapitalQ]1][r]^2),{r,1,n[\[ScriptCapitalQ]1]}];
Subscript[wC, qe][i_,j_,k_,l_]:=-Sum[(Conjugate[y[\[CapitalPi]7][eq][r,j,k]])(y[\[CapitalPi]7][eq][r,i,l])/(2M[\[CapitalPi]7][r]^2),{r,1,n[\[CapitalPi]7]}]-Sum[g[B][e][r,k,l]g[B][q][r,i,j]/(M[B][r]^2),{r,1,n[B]}]+Sum[Conjugate[g[\[ScriptCapitalQ]5][eq][r,k,i]](g[\[ScriptCapitalQ]5][eq][r,l,j])/(M[\[ScriptCapitalQ]5][r]^2),{r,1,n[\[ScriptCapitalQ]5]}];
Subscript[wC, qu1][i_,j_,k_,l_]:=-Sum[(Conjugate[y[\[CurlyPhi]][u][r,j,k]])(y[\[CurlyPhi]][u][r,i,l])/(6M[\[CurlyPhi]][r]^2),{r,1,n[\[CurlyPhi]]}]-2Sum[Conjugate[y[\[CapitalPhi]][qu][r,j,k]]y[\[CapitalPhi]][qu][r,i,l]/(9M[\[CapitalPhi]][r]^2),{r,1,n[\[CapitalPhi]]}]-Sum[g[B][u][r,k,l]g[B][q][r,i,j]/(M[B][r]^2),{r,1,n[B]}]+2Sum[Conjugate[g[\[ScriptCapitalQ]5][uq][r,l,j]](g[\[ScriptCapitalQ]5][uq][r,k,i])/(3M[\[ScriptCapitalQ]5][r]^2),{r,1,n[\[ScriptCapitalQ]5]}]+2Sum[Conjugate[g[Y5][r,l,j]](g[Y5][r,k,i])/(3M[Y5][r]^2),{r,1,n[Y5]}]+If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[(yHat[u][k,j]\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]y[\[CurlyPhi]][u][s,i,l]+Conjugate[yHat[u][l,i]]Conjugate[\[Delta][L1,\[CurlyPhi]][r,s]]\[Gamma][L1][r]Conjugate[y[\[CurlyPhi]][u][s,j,k]])/(6M[L1][r]^2*M[\[CurlyPhi]][s]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]}],0]-If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[yHat[u][k,j]Conjugate[yHat[u][l,i]]Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]\[Gamma][L1][t]\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]/(6M[L1][r]^2 M[\[CurlyPhi]][s]^2 M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}],0];
Subscript[wC, qu8][i_,j_,k_,l_]:=-Sum[(Conjugate[y[\[CurlyPhi]][u][r,j,k]])(y[\[CurlyPhi]][u][r,i,l])/(M[\[CurlyPhi]][r]^2),{r,1,n[\[CurlyPhi]]}]+Sum[Conjugate[y[\[CapitalPhi]][qu][r,j,k]]y[\[CapitalPhi]][qu][r,i,l]/(6M[\[CapitalPhi]][r]^2),{r,1,n[\[CapitalPhi]]}]-Sum[g[G][u][r,k,l]g[G][q][r,i,j]/(M[G][r]^2),{r,1,n[G]}]-2Sum[Conjugate[g[\[ScriptCapitalQ]5][uq][r,l,j]](g[\[ScriptCapitalQ]5][uq][r,k,i])/(M[\[ScriptCapitalQ]5][r]^2),{r,1,n[\[ScriptCapitalQ]5]}]+Sum[Conjugate[g[Y5][r,l,j]](g[Y5][r,k,i])/(M[Y5][r]^2),{r,1,n[Y5]}]+If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[(yHat[u][k,j]\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]y[\[CurlyPhi]][u][s,i,l]+Conjugate[yHat[u][l,i]]Conjugate[\[Delta][L1,\[CurlyPhi]][r,s]]\[Gamma][L1][r]Conjugate[y[\[CurlyPhi]][u][s,j,k]])/(M[L1][r]^2*M[\[CurlyPhi]][s]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]}],0]-If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[yHat[u][k,j]Conjugate[yHat[u][l,i]]Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]*\[Gamma][L1][t]*\[Delta][L1,\[CurlyPhi]][r,s]*Conjugate[\[Gamma][L1][r]]/(M[L1][r]^2 M[\[CurlyPhi]][s]^2 M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}],0];
Subscript[wC, qd1][i_,j_,k_,l_]:=-Sum[(Conjugate[y[\[CurlyPhi]][d][r,l,i]])(y[\[CurlyPhi]][d][r,k,j])/(6M[\[CurlyPhi]][r]^2),{r,1,n[\[CurlyPhi]]}]-2Sum[Conjugate[y[\[CapitalPhi]][dq][r,l,i]]y[\[CapitalPhi]][dq][r,k,j]/(9M[\[CapitalPhi]][r]^2),{r,1,n[\[CapitalPhi]]}]-Sum[g[B][d][r,k,l]g[B][q][r,i,j]/(M[B][r]^2),{r,1,n[B]}]+2Sum[Conjugate[g[\[ScriptCapitalQ]1][dq][r,l,j]](g[\[ScriptCapitalQ]1][dq][r,k,i])/(3M[\[ScriptCapitalQ]1][r]^2),{r,1,n[\[ScriptCapitalQ]1]}]+2Sum[Conjugate[g[Y1][r,l,j]](g[Y1][r,k,i])/(3M[Y1][r]^2),{r,1,n[Y1]}]-If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[(Conjugate[yHat[d][l,i]]\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]y[\[CurlyPhi]][d][s,k,j]+yHat[d][k,j]Conjugate[\[Delta][L1,\[CurlyPhi]][r,s]]\[Gamma][L1][r]Conjugate[y[\[CurlyPhi]][d][s,l,i]])/(6M[L1][r]^2*M[\[CurlyPhi]][s]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]}],0]-If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[yHat[d][k,j]Conjugate[yHat[d][l,i]]Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]\[Gamma][L1][t]*\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]/(6M[L1][r]^2 M[\[CurlyPhi]][s]^2 M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}],0];
Subscript[wC, qd8][i_,j_,k_,l_]:=-Sum[(Conjugate[y[\[CurlyPhi]][d][r,l,i]])(y[\[CurlyPhi]][d][r,k,j])/(M[\[CurlyPhi]][r]^2),{r,1,n[\[CurlyPhi]]}]+Sum[Conjugate[y[\[CapitalPhi]][dq][r,l,i]]y[\[CapitalPhi]][dq][r,k,j]/(6M[\[CapitalPhi]][r]^2),{r,1,n[\[CapitalPhi]]}]-Sum[g[G][d][r,k,l]g[G][q][r,i,j]/(M[G][r]^2),{r,1,n[G]}]-2Sum[Conjugate[g[\[ScriptCapitalQ]1][dq][r,l,j]](g[\[ScriptCapitalQ]1][dq][r,k,i])/(M[\[ScriptCapitalQ]1][r]^2),{r,1,n[\[ScriptCapitalQ]1]}]+Sum[Conjugate[g[Y1][r,l,j]](g[Y1][r,k,i])/(M[Y1][r]^2),{r,1,n[Y1]}]+If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[(yHat[d][k,j]Conjugate[\[Delta][L1,\[CurlyPhi]][r,s]]\[Gamma][L1][r]Conjugate[y[\[CurlyPhi]][d][s,l,i]]+Conjugate[yHat[d][l,i]]\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]Conjugate[y[\[CurlyPhi]][d][s,k,j]])/(M[L1][r]^2*M[\[CurlyPhi]][s]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]}],0]-If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[yHat[d][k,j]Conjugate[yHat[d][l,i]]Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]*\[Gamma][L1][t]*\[Delta][L1,\[CurlyPhi]][r,s]*Conjugate[\[Gamma][L1][r]]/(M[L1][r]^2 M[\[CurlyPhi]][s]^2 M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}],0];
Subscript[wC, ledq][i_,j_,k_,l_]:=Sum[y[\[CurlyPhi]][d][r,k,l]Conjugate[y[\[CurlyPhi]][e][r,j,i]]/(M[\[CurlyPhi]][r]^2),{r,1,n[\[CurlyPhi]]}]+2Sum[g[\[ScriptCapitalU]2][Lq][r,i,l]Conjugate[g[\[ScriptCapitalU]2][ed][r,j,k]]/(M[\[ScriptCapitalU]2][r]^2),{r,1,n[\[ScriptCapitalU]2]}]-2Sum[g[\[ScriptCapitalQ]5][eq][r,j,l]Conjugate[g[\[ScriptCapitalQ]5][dL][r,k,i]]/(M[\[ScriptCapitalQ]5][r]^2),{r,1,n[\[ScriptCapitalQ]5]}]+If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[(Conjugate[yHat[e][j,i]]\[Delta][L1,\[CurlyPhi]][s,r]Conjugate[\[Gamma][L1][s]]y[\[CurlyPhi]][d][r,k,l]+yHat[d][k,l]Conjugate[\[Delta][L1,\[CurlyPhi]][s,r]]\[Gamma][L1][s]Conjugate[y[\[CurlyPhi]][e][r,j,i]])/(M[\[CurlyPhi]][r]^2 M[L1][s]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[L1]}],0]+If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[yHat[d][k,l]Conjugate[yHat[e][j,i]]Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]\[Gamma][L1][t]\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]/(M[L1][r]^2*M[\[CurlyPhi]][s]^2*M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}],0];
Subscript[wC, quqd1][i_,j_,k_,l_]:=-Sum[y[\[CurlyPhi]][u][r,i,j]Conjugate[y[\[CurlyPhi]][d][r,l,k]]/(M[\[CurlyPhi]][r]^2),{r,1,n[\[CurlyPhi]]}]+4Sum[y[\[Omega]1][qq][r,k,i]Conjugate[y[\[Omega]1][du][r,l,j]]/(3M[\[Omega]1][r]^2),{r,1,n[\[Omega]1]}]+4Sum[y[\[CapitalOmega]1][ud][r,j,l]Conjugate[y[\[CapitalOmega]1][qq][r,k,i]]/(M[\[CapitalOmega]1][r]^2),{r,1,n[\[CapitalOmega]1]}]+If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[(Conjugate[yHat[u][j,i]]Conjugate[\[Delta][L1,\[CurlyPhi]]][s,r]\[Gamma][L1][s]Conjugate[y[\[CurlyPhi]][d][r,k,l]]-Conjugate[yHat[d][l,k]]\[Delta][L1,\[CurlyPhi]][s,r]Conjugate[\[Gamma][L1][s]]y[\[CurlyPhi]][u][r,i,j])/(M[\[CurlyPhi]][r]^2 M[L1][s]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[L1]}],0]+If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[Conjugate[yHat[d][l,k]]Conjugate[yHat[u][j,i]]Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]\[Gamma][L1][t]\[Delta][L1,\[CurlyPhi]][r,s]*Conjugate[\[Gamma][L1][r]]/(M[L1][r]^2*M[\[CurlyPhi]][s]^2*M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}],0];
Subscript[wC, quqd8][i_,j_,k_,l_]:=-4Sum[y[\[Omega]1][qq][r,k,i]Conjugate[y[\[Omega]1][du][r,l,j]]/(M[\[Omega]1][r]^2),{r,1,n[\[Omega]1]}]+2Sum[y[\[CapitalOmega]1][ud][r,j,l]Conjugate[y[\[CapitalOmega]1][qq][r,k,i]]/(M[\[CapitalOmega]1][r]^2),{r,1,n[\[CapitalOmega]1]}]-Sum[y[\[CapitalPhi]][dq][r,l,k]Conjugate[y[\[CapitalPhi]][qu][r,i,j]]/(M[\[CapitalPhi]][r]^2),{r,1,n[\[CapitalPhi]]}];
Subscript[wC, lequ1][i_,j_,k_,l_]:=Sum[y[\[CurlyPhi]][u][r,k,l]Conjugate[y[\[CurlyPhi]][e][r,j,i]]/(M[\[CurlyPhi]][r]^2),{r,1,n[\[CurlyPhi]]}]+Sum[y[\[Omega]1][eu][r,j,l]Conjugate[y[\[Omega]1][qL][r,k,i]]/(2M[\[Omega]1][r]^2),{r,1,n[\[Omega]1]}]+Sum[Conjugate[y[\[CapitalPi]7][eq][r,j,k]]y[\[CapitalPi]7][Lu][r,i,l]/(2),{r,1,n[\[CapitalPi]7]}]+If[n[L1]>0&&n[\[CurlyPhi]]>0,Sum[(Conjugate[yHat[e][j,i]]\[Delta][L1,\[CurlyPhi]][s,r]Conjugate[\[Gamma][L1][s]]y[\[CurlyPhi]][u][r,k,l]-Conjugate[yHat[u][l,k]]Conjugate[\[Delta][L1,\[CurlyPhi]][s,r]]\[Gamma][L1][s]Conjugate[y[\[CurlyPhi]][e][r,j,i]])/(M[\[CurlyPhi]][r]^2*M[L1][s]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[L1]}]-Sum[Conjugate[yHat[u][l,k]]Conjugate[yHat[e][j,i]]Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]\[Gamma][L1][t]\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]],{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}],0];
Subscript[wC, lequ3][i_,j_,k_,l_]:=-Sum[y[\[Omega]1][eu][r,j,l]Conjugate[y[\[Omega]1][qL][r,k,i]]/(8M[\[Omega]1][r]^2),{r,1,n[\[Omega]1]}]+Sum[y[\[CapitalPi]7][eq][r,j,k]Conjugate[y[\[CapitalPi]7][Lu][r,k,i]]/(8M[\[CapitalPi]7][r]^2),{r,1,n[\[CapitalPi]7]}];
Subscript[wC, duq][i_,j_,k_,l_]:=Sum[Conjugate[y[\[Omega]1][du][r,i,j]]y[\[Omega]1][qL][r,k,l]/(M[\[Omega]1][r]^2),{r,1,n[\[Omega]1]}]+2Sum[Conjugate[g[\[ScriptCapitalQ]1][dq][r,i,k]g[\[ScriptCapitalQ]1][uL][r,j,l]]/(M[\[ScriptCapitalQ]1][r]^2),{r,1,n[\[ScriptCapitalQ]1]}]-2Sum[Conjugate[g[\[ScriptCapitalQ]5][uq][r,j,k]]g[\[ScriptCapitalQ]5][dL][r,i,l]/(M[\[ScriptCapitalQ]5][r]^2),{r,1,n[\[ScriptCapitalQ]5]}];
Subscript[wC, qqu][i_,j_,k_,l_]:=Sum[Conjugate[y[\[Omega]1][qq][r,i,j]]y[\[Omega]1][eu][r,l,k]/(M[\[Omega]1][r]^2),{r,1,n[\[Omega]1]}]-2Sum[Conjugate[g[\[ScriptCapitalQ]5][uq][r,k,i]g[\[ScriptCapitalQ]5][eq][r,l,j]]/(M[\[ScriptCapitalQ]5][r]^2),{r,1,n[\[ScriptCapitalQ]5]}];
Subscript[wC, qqq][i_,j_,k_,l_]:=2Sum[Conjugate[y[\[Omega]1][qq][r,i,j]]y[\[Omega]1][qL][r,k,l]/(M[\[Omega]1][r]^2),{r,1,n[\[Omega]1]}]-2Sum[Conjugate[y[\[Zeta]][qq][r,i,j]y[\[Zeta]][qL][r,k,l]]/(M[\[Zeta]][r]^2),{r,1,n[\[Zeta]]}];
Subscript[wC, duu][i_,j_,k_,l_]:=Sum[Conjugate[y[\[Omega]1][du][r,i,j]]y[\[Omega]1][eu][r,l,k]/(M[\[Omega]1][r]^2),{r,1,n[\[Omega]1]}]-2Sum[Conjugate[y[\[Omega]4][uu][r,j,k]]y[\[Omega]4][ed][r,l,i]/(M[\[Omega]4][r]^2),{r,1,n[\[Omega]4]}];
Subscript[wC, W]:=0;
Subscript[wC, Wtil]:=0;
Subscript[wC, G]:=0;
Subscript[wC, Gtil]:=0;
Subscript[wC, \[Phi]V]:=-2\[Lambda]Hat[\[Phi]]Sum[Conjugate[gHat[B1][\[Phi]][r]]gHat[B1][\[Phi]][r]/(M[B1][r]^2),{r,1,n[B1]}]-\[Lambda]Hat[\[Phi]]*Sum[Conjugate[gHat[W][\[Phi]][r]]gHat[W][\[Phi]][r]/(M[W][r]^2),{r,1,n[W]}]-\[Lambda]Hat[\[Phi]]*Sum[Conjugate[gHat[W1][\[Phi]][r]]gHat[W1][\[Phi]][r]/(2M[W1][r]^2),{r,1,n[W1]}]+2Subscript[g2, SM] \[Lambda]Hat[\[Phi]]*Sum[g[L1][W][s,r]Conjugate[\[Gamma][L1][s]]\[Gamma][L1][r]/(M[L1][r]^2 M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]-2Subscript[\[Lambda]Hat, \[Phi]]*Sum[g1[L1][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(M[L1][r]^2 M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}];
Subscript[wC, \[Phi]S]:=-Sum[\[Lambda][S][r,s]\[Kappa][S][r]\[Kappa][S][s]/(M[S][r]^2M[S][s]^2),{r,1,n[S]},{s,1,n[S]}]+Sum[\[Kappa]3[S][r,t,s]\[Kappa][S][r]\[Kappa][S][s]\[Kappa][S][t]/(M[S][r]^2M[S][s]^2M[S][t]^2),{r,1,n[S]},{s,1,n[S]},{t,1,n[S]}]+Sum[Conjugate[\[Lambda][\[CurlyPhi]][r]]\[Lambda][\[CurlyPhi]][r]/(M[\[CurlyPhi]][r]^2),{r,1,n[\[CurlyPhi]]}]+4 \[Lambda]Hat[\[Phi]]Sum[\[Kappa][\[CapitalXi]][r]\[Kappa][\[CapitalXi]][r]/(M[\[CapitalXi]][r]^4),{r,1,n[\[CapitalXi]]}]-Sum[\[Lambda][\[CapitalXi]][s]\[Kappa][\[CapitalXi]][s]\[Kappa][\[CapitalXi]][r]/(M[\[CapitalXi]][r]^2M[\[CapitalXi]][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[\[CapitalXi]]}]+8\[Lambda]Hat[\[Phi]]Sum[Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Kappa][\[CapitalXi]1][r]/(M[\[CapitalXi]1][r]^4),{r,1,n[\[CapitalXi]1]}]-2Sum[\[Lambda][\[CapitalXi]1][r,s]Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Kappa][\[CapitalXi]1][s]/(M[\[CapitalXi]1][r]^2M[\[CapitalXi]1][s]^2),{r,1,n[\[CapitalXi]1]},{s,1,n[\[CapitalXi]1]}]+Sqrt[2]Sum[\[Lambda]p[\[CapitalXi]1][r,s]Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Kappa][\[CapitalXi]1][s]/(M[\[CapitalXi]1][r]^2M[\[CapitalXi]1][s]^2),{r,1,n[\[CapitalXi]1]},{s,1,n[\[CapitalXi]1]}]+Sum[Conjugate[\[Lambda][\[CapitalTheta]1][r]]\[Lambda][\[CapitalTheta]1][r]/(6 M[\[CapitalTheta]1][r]^2),{r,1,n[\[CapitalTheta]1]}]+Sum[Conjugate[\[Lambda][\[CapitalTheta]3][r]]\[Lambda][\[CapitalTheta]3][r]/(6 M[\[CapitalTheta]3][r]^2),{r,1,n[\[CapitalTheta]3]}]-2If[n[S]>0&&n[\[CurlyPhi]]>0,Sum[Re[\[Kappa][S,\[CurlyPhi]][r,s]Conjugate[\[Lambda][\[CurlyPhi]][s]]]\[Kappa][S][r]/(M[S][r]^2M[\[CurlyPhi]][s]^2),{r,1,n[S]},{s,1,n[\[CurlyPhi]]}],0]+If[n[S]>0&&n[\[CurlyPhi]]>0,Sum[Conjugate[\[Kappa][S,\[CurlyPhi]][r,t]]\[Kappa][S,\[CurlyPhi]][s,t]\[Kappa][S][r]\[Kappa][S][s]/(M[S][r]^2M[S][s]^2M[\[CurlyPhi]][t]^2),{r,1,n[S]},{s,1,n[S]},{t,1,n[\[CurlyPhi]]}],0]-If[n[S]>0&&n[\[CapitalXi]]>0,Sum[\[Lambda][\[CapitalXi]][s,r]\[Kappa][S][s]\[Kappa][\[CapitalXi]][r]/(M[\[CapitalXi]][r]^2M[S][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[S]}],0]+If[n[S]>0&&n[\[CapitalXi]]>0,Sum[\[Kappa][S,\[CapitalXi]][t,s,r]\[Kappa][S][t]\[Kappa][\[CapitalXi]][s]\[Kappa][\[CapitalXi]][r]/(M[\[CapitalXi]][r]^2M[\[CapitalXi]][s]^2M[S][t]^2),{r,1,n[\[CapitalXi]]},{s,1,n[\[CapitalXi]]},{t,1,n[S]}],0]-2If[n[\[CapitalXi]]>0&&n[\[CurlyPhi]]>0,Sum[Re[\[Kappa][\[CapitalXi],\[CurlyPhi]][r,s]Conjugate[\[Lambda][\[CurlyPhi]][s]]]\[Kappa][\[CapitalXi]][r]/(M[\[CapitalXi]][r]^2M[\[CurlyPhi]][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[\[CurlyPhi]]}],0]+If[n[\[CurlyPhi]]>0&&n[\[CapitalXi]]>0,Sum[Conjugate[\[Kappa][\[CapitalXi],\[CurlyPhi]][t,r]]\[Kappa][\[CapitalXi],\[CurlyPhi]][s,r]\[Kappa][\[CapitalXi]][t]\[Kappa][\[CapitalXi]][s]/(M[\[CurlyPhi]][r]^2M[\[CapitalXi]][s]^2M[\[CapitalXi]][t]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[\[CapitalXi]]},{t,1,n[\[CapitalXi]]}],0]-4If[n[S]>0&&n[\[CapitalXi]1]>0,Sum[Re[\[Lambda][S,\[CapitalXi]1][r,s]Conjugate[\[Kappa][\[CapitalXi]1][s]]]\[Kappa][S][r]/(M[\[CapitalXi]1][s]^2M[S][r]^2),{s,1,n[\[CapitalXi]1]},{r,1,n[S]}],0]+2If[n[S]>0&&n[\[CapitalXi]1]>0,Sum[\[Kappa][S,\[CapitalXi]1][t,r,s]\[Kappa][S][t]Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Kappa][\[CapitalXi]1][s]/(M[\[CapitalXi]1][r]^2M[\[CapitalXi]1][s]^2M[S][t]^2),{r,1,n[\[CapitalXi]1]},{s,1,n[\[CapitalXi]1]},{t,1,n[S]}],0]-2Sqrt[2]If[n[\[CapitalXi]1]>0&&n[\[CapitalXi]]>0,Sum[Re[\[Lambda][\[CapitalXi]1,\[CapitalXi]][r,s]Conjugate[\[Kappa][\[CapitalXi]1][r]]]\[Kappa][\[CapitalXi]][s]/(M[\[CapitalXi]][s]^2M[\[CapitalXi]1][r]^2),{s,1,n[\[CapitalXi]]},{r,1,n[\[CapitalXi]1]}],0]-Sqrt[2]If[n[\[CapitalXi]1]>0&&n[\[CapitalXi]]>0,Sum[\[Kappa][\[CapitalXi],\[CapitalXi]1][s,r,t]\[Kappa][\[CapitalXi]][s]Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Kappa][\[CapitalXi]1][t]/(M[\[CapitalXi]1][r]^2M[\[CapitalXi]][s]^2M[\[CapitalXi]1][t]^2),{r,1,n[\[CapitalXi]1]},{s,1,n[\[CapitalXi]]},{t,1,n[\[CapitalXi]1]}],0]-4If[n[\[CapitalXi]1]>0&&n[\[CurlyPhi]]>0,Sum[Re[Conjugate[\[Kappa][\[CapitalXi]1,\[CurlyPhi]][r,s]]\[Kappa][\[CapitalXi]1][r]Conjugate[\[Lambda][\[CurlyPhi]][s]]]/(M[\[CapitalXi]1][r]^2M[\[CurlyPhi]][s]^2),{r,1,n[\[CapitalXi]1]},{s,1,n[\[CurlyPhi]]}],0]+4If[n[\[CapitalXi]1]>0&&n[\[CurlyPhi]]>0,Sum[Conjugate[\[Kappa][\[CapitalXi]1,\[CurlyPhi]][s,t]]\[Kappa][\[CapitalXi]1][s]\[Kappa][\[CapitalXi]1,\[CurlyPhi]][r,t]Conjugate[\[Kappa][\[CapitalXi]1][r]]/(M[\[CapitalXi]1][r]^2M[\[CapitalXi]1][s]^2M[\[CurlyPhi]][t]^2),{r,1,n[\[CapitalXi]1]},{s,1,n[\[CapitalXi]1]},{t,1,n[\[CurlyPhi]]}],0]-If[n[\[CapitalXi]]>0&&n[\[CapitalTheta]1]>0,Sum[Re[\[Kappa][\[CapitalXi],\[CapitalTheta]1][r,s]Conjugate[\[Lambda][\[CapitalTheta]1][s]]](\[Kappa][\[CapitalXi]][r])/(3M[\[CapitalTheta]1][s]^2M[\[CapitalXi]][r]^2),{s,1,n[\[CapitalTheta]1]},{r,1,n[\[CapitalXi]]}],0]+If[n[\[CapitalXi]]>0&&n[\[CapitalTheta]1]>0,Sum[Conjugate[\[Kappa][\[CapitalXi],\[CapitalTheta]1][r,s]]\[Kappa][\[CapitalXi]][r]\[Kappa][\[CapitalXi],\[CapitalTheta]1][t,s]\[Kappa][\[CapitalXi]][t]/(6M[\[CapitalXi]][r]^2M[\[CapitalTheta]1][s]^2M[\[CapitalXi]][t]^2),{r,1,n[\[CapitalXi]]},{s,1,n[\[CapitalTheta]1]},{t,1,n[\[CapitalXi]]}],0]-If[n[\[CapitalXi]1]>0&&n[\[CapitalTheta]1]>0,Sum[Re[\[Kappa][\[CapitalXi]1,\[CapitalTheta]1][r,s]Conjugate[\[Kappa][\[CapitalXi]1][r]]Conjugate[\[Lambda][\[CapitalTheta]1][s]]]/(3M[\[CapitalXi]1][r]^2M[\[CapitalTheta]1][s]^2),{r,1,n[\[CapitalXi]1]},{s,1,n[\[CapitalTheta]1]}],0]+If[n[\[CapitalXi]1]>0&&n[\[CapitalTheta]1]>0,Sum[Conjugate[\[Kappa][\[CapitalXi]1,\[CapitalTheta]1][t,r]]\[Kappa][\[CapitalXi]1][t]\[Kappa][\[CapitalXi]1,\[CapitalTheta]1][s,r]Conjugate[\[Kappa][\[CapitalXi]1][s]]/(6M[\[CapitalXi]1][s]^2M[\[CapitalTheta]1][r]^2M[\[CapitalXi]1][t]^2),{s,1,n[\[CapitalXi]1]},{r,1,n[\[CapitalTheta]1]},{t,1,n[\[CapitalXi]1]}],0]-If[n[\[CapitalXi]1]>0&&n[\[CapitalTheta]3]>0,Sum[Re[\[Kappa][\[CapitalXi]1,\[CapitalTheta]3][r,s]Conjugate[\[Kappa][\[CapitalXi]1][r]]Conjugate[\[Lambda][\[CapitalTheta]3][s]]]/(M[\[CapitalXi]1][r]^2M[\[CapitalTheta]3][s]^2),{r,1,n[\[CapitalXi]1]},{s,1,n[\[CapitalTheta]3]}],0]+If[n[\[CapitalXi]1]>0&&n[\[CapitalTheta]3]>0,Sum[Conjugate[\[Kappa][\[CapitalXi]1,\[CapitalTheta]3][t,r]]\[Kappa][\[CapitalXi]1][t]\[Kappa][\[CapitalXi]1,\[CapitalTheta]3][s,r]Conjugate[\[Kappa][\[CapitalXi]1][s]]/(2M[\[CapitalXi]1][s]^2M[\[CapitalTheta]3][r]^2M[\[CapitalXi]1][t]^2),{s,1,n[\[CapitalXi]1]},{r,1,n[\[CapitalTheta]3]},{t,1,n[\[CapitalXi]1]}],0]+2If[n[\[CurlyPhi]]>0&&n[S]>0&&n[\[CapitalXi]]>0,Sum[Re[Conjugate[\[Kappa][\[CapitalXi],\[CurlyPhi]][r,s]]\[Kappa][S,\[CurlyPhi]][t,s]]\[Kappa][\[CapitalXi]][r]\[Kappa][S][t]/(M[\[CapitalXi]][r]^2M[S][t]^2M[\[CurlyPhi]][s]^2),{r,1,n[\[CapitalXi]]},{t,1,n[S]},{s,1,n[\[CurlyPhi]]}],0]+4If[n[\[CurlyPhi]]>0&&n[\[CapitalXi]1]>0&&n[S]>0,Sum[Re[\[Kappa][\[CapitalXi]1,\[CurlyPhi]][r,s]Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Kappa][S,\[CurlyPhi]][t,s]]\[Kappa][S][t]/(M[\[CurlyPhi]][s]^2M[\[CapitalXi]1][r]^2M[S][t]^2),{s,1,n[\[CurlyPhi]]},{r,1,n[\[CapitalXi]1]},{t,1,n[S]}],0]+4If[n[\[CapitalXi]1]>0&&n[\[CapitalXi]]>0&&n[\[CurlyPhi]]>0,Sum[Re[Conjugate[\[Kappa][\[CapitalXi]1,\[CurlyPhi]][r,s]]\[Kappa][\[CapitalXi]1][r]Conjugate[\[Kappa][\[CapitalXi],\[CurlyPhi]][t,s]]]\[Kappa][\[CapitalXi]][t]/(M[\[CurlyPhi]][s]^2M[\[CapitalXi]1][r]^2M[\[CapitalXi]][t]^2),{s,1,n[\[CurlyPhi]]},{r,1,n[\[CapitalXi]1]},{t,1,n[\[CapitalXi]]}],0]+If[n[\[CapitalXi]1]>0&&n[\[CapitalTheta]1]>0&&n[\[CapitalXi]]>0,Sum[Re[Conjugate[\[Kappa][\[CapitalXi]1,\[CapitalTheta]1][r,s]]\[Kappa][\[CapitalXi]1][r]\[Kappa][\[CapitalXi],\[CapitalTheta]1][t,s]]\[Kappa][\[CapitalXi]][t]/(3M[\[CapitalTheta]1][s]^2M[\[CapitalXi]1][r]^2M[\[CapitalXi]][t]^2),{s,1,n[\[CapitalTheta]1]},{r,1,n[\[CapitalXi]1]},{t,1,n[\[CapitalXi]]}],0]
Subscript[wC, \[Phi]SV]:=\[Lambda]Hat[\[Phi]]*(-4Sum[Im[gHat[W][\[Phi]][r]]\[Delta][W,\[CapitalXi]][r,s]\[Kappa][\[CapitalXi]][s]/(M[W][r]^2M[\[CapitalXi]][s]^2),{r,1,n[W]},{s,1,n[\[CapitalXi]]}]-4Sum[\[Delta][W,\[CapitalXi]][t,s]\[Delta][W,\[CapitalXi]][t,r]\[Kappa][\[CapitalXi]][r]\[Kappa][\[CapitalXi]][s]/(M[\[CapitalXi]][r]^2M[\[CapitalXi]][s]^2M[W][t]^2),{r,1,n[\[CapitalXi]]},{s,1,n[\[CapitalXi]]},{t,1,n[W]}]-4Sum[Im[gHat[W1][\[Phi]][r]\[Delta][W1,\[CapitalXi]1][r,s]\[Kappa][\[CapitalXi]1][s]]/(M[W1][r]^2M[\[CapitalXi]1][s]^2),{r,1,n[W1]},{s,1,n[\[CapitalXi]1]}]-8If[n[\[CapitalXi]1]>0&&n[W1]>0,Sum[Conjugate[\[Delta][W1,\[CapitalXi]1][s,t]]\[Delta][W1,\[CapitalXi]1][s,r]\[Kappa][\[CapitalXi]1][r]Conjugate[\[Kappa][\[CapitalXi]1][t]]/(M[\[CapitalXi]1][r]^2M[\[CapitalXi]1][t]^2M[W1][s]^2),{r,1,n[\[CapitalXi]1]},{t,1,n[\[CapitalXi]1]},{s,1,n[W1]}],0]+4If[n[L1]>0&&n[S]>0,Sum[Re[g2[S,L1][r,s]\[Gamma][L1][s]]\[Kappa][S][r]/(M[S][r]^2 M[L1][s]^2),{r,1,n[S]},{s,1,n[L1]}],0]+2Sum[\[CurlyEpsilon][S,L1][r,t,s]\[Kappa][S][r]Conjugate[\[Gamma][L1][t]]\[Gamma][L1][s]/(M[S][r]^2M[L1][s]^2M[L1][t]^2),{r,1,n[S]},{s,1,n[L1]},{t,1,n[L1]}]+4Sum[Re[\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]\[Lambda][\[CurlyPhi]][s]]/(M[\[CurlyPhi]][s]^2M[L1][r]^2),{s,1,n[\[CurlyPhi]]},{r,1,n[L1]}]+4Sum[Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]\[Gamma][L1][t]\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]/(M[L1][r]^2M[\[CurlyPhi]][s]^2M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}]-4Sum[Re[g2[\[CapitalXi],L1][r,s]\[Gamma][L1][s]]\[Kappa][\[CapitalXi]][r]/(M[\[CapitalXi]][r]^2M[L1][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[L1]}]-2Sum[\[CurlyEpsilon][\[CapitalXi],L1][s,r,t]\[Kappa][\[CapitalXi]][s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][t]/(M[L1][r]^2M[\[CapitalXi]][s]^2M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CapitalXi]]},{t,1,n[L1]}]+8Sum[Re[Conjugate[g1[\[CapitalXi],L1][r,s]]Conjugate[\[Gamma][L1][s]]]\[Kappa][\[CapitalXi]][r]/(M[\[CapitalXi]][r]^2M[L1][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[L1]}]+8Sum[Re[Conjugate[g1[\[CapitalXi]1,L1][r,s]]Conjugate[\[Gamma][L1][s]]\[Kappa][\[CapitalXi]1][r]]/(M[L1][s]^2M[\[CapitalXi]1][r]^2),{s,1,n[L1]},{r,1,n[\[CapitalXi]1]}]-4Sum[Re[\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]\[Kappa][S,\[CurlyPhi]][t,s]]\[Kappa][S][t]/(M[\[CurlyPhi]][s]^2M[S][t]^2M[L1][r]^2),{s,1,n[\[CurlyPhi]]},{t,1,n[S]},{r,1,n[L1]}]-4Sum[Re[\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]\[Kappa][\[CapitalXi],\[CurlyPhi]][t,s]]\[Kappa][\[CapitalXi]][t]/(M[\[CurlyPhi]][s]^2M[\[CapitalXi]][t]^2M[L1][r]^2),{s,1,n[\[CurlyPhi]]},{t,1,n[\[CapitalXi]]},{r,1,n[L1]}]-8Sum[Re[\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]Conjugate[\[Kappa][\[CapitalXi]1,\[CurlyPhi]][t,s]]\[Kappa][\[CapitalXi]1][t]]/(M[\[CurlyPhi]][s]^2M[\[CapitalXi]1][t]^2M[L1][r]^2),{s,1,n[\[CurlyPhi]]},{t,1,n[\[CapitalXi]1]},{r,1,n[L1]}]);
Subscript[wC, \[Phi]]:=(Subscript[wC, \[Phi]S]+Subscript[wC, \[Phi]V]+Subscript[wC, \[Phi]SV])/(Z\[Phi]^3)
Subscript[wC, \[Phi]D]:=(-2Sum[(\[Kappa][\[CapitalXi]][r])(\[Kappa][\[CapitalXi]][r])/(M[\[CapitalXi]][r]^4),{r,1,n[\[CapitalXi]]}]+4Sum[Conjugate[(\[Kappa][\[CapitalXi]1][r])](\[Kappa][\[CapitalXi]1][r])/(M[\[CapitalXi]1][r]^4),{r,1,n[\[CapitalXi]1]}]-Sum[Re[gHat[B][\[Phi]][r]gHat[B][\[Phi]][r]]/(M[B][r]^2),{r,1,n[B]}]-Sum[Conjugate[gHat[B][\[Phi]][r]]gHat[B][\[Phi]][r]/(M[B][r]^2),{r,1,n[B]}]+Sum[Conjugate[gHat[B1][\[Phi]][r]]gHat[B1][\[Phi]][r]/(M[B1][r]^2),{r,1,n[B1]}]-Sum[Re[gHat[W][\[Phi]][r]gHat[W][\[Phi]][r]]/(4*M[W][r]^2),{r,1,n[W]}]+Sum[Conjugate[gHat[W][\[Phi]][r]]gHat[W][\[Phi]][r]/(4*M[W][r]^2),{r,1,n[W]}]-Sum[Conjugate[gHat[W1][\[Phi]][r]]gHat[W1][\[Phi]][r]/(4M[W1][r]^2),{r,1,n[W1]}]+Sum[Subscript[g1, SM] g[L1][B][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]-Sum[h2[L1][r,s] Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]+2Sum[Re[ h2[L1][r,s]Conjugate[\[Gamma][L1][r]]Conjugate[\[Gamma][L1][s]]]/(M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]+2Sum[Im[gHat[W][\[Phi]][r]]\[Delta][W,\[CapitalXi]][r,s]\[Kappa][\[CapitalXi]][s]/(M[W][r]^2M[\[CapitalXi]][s]^2),{r,1,n[W]},{s,1,n[\[CapitalXi]]}]+2Sum[\[Delta][W,\[CapitalXi]][t,s]\[Delta][W,\[CapitalXi]][t,r]\[Kappa][\[CapitalXi]][r]\[Kappa][\[CapitalXi]][s]/(M[\[CapitalXi]][r]^2M[\[CapitalXi]][s]^2M[W][t]^2),{r,1,n[\[CapitalXi]]},{s,1,n[\[CapitalXi]]},{t,1,n[W]}]-2Sum[Im[Conjugate[gHat[W1][\[Phi]][r]]\[Delta][W1,\[CapitalXi]1][r,s]\[Kappa][\[CapitalXi]1][s]]/(M[W1][r]^2M[\[CapitalXi]1][s]^2),{r,1,n[W1]},{s,1,n[\[CapitalXi]1]}]-4Sum[Conjugate[\[Delta][W1,\[CapitalXi]1][t,r]]Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Kappa][\[CapitalXi]1][s]\[Delta][W1,\[CapitalXi]1][t,s]/(M[\[CapitalXi]1][r]^2M[\[CapitalXi]1][s]^2M[W1][t]^2),{r,1,n[\[CapitalXi]1]},{s,1,n[\[CapitalXi]1]},{t,1,n[W1]}]-4Sum[Re[Conjugate[g1[\[CapitalXi],L1][r,s]]Conjugate[\[Gamma][L1][s]]]\[Kappa][\[CapitalXi]][r]/(M[\[CapitalXi]][r]^2M[L1][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[L1]}]+4Sum[Re[g2[\[CapitalXi],L1][r,s] \[Gamma][L1][s]]\[Kappa][\[CapitalXi]][r]/(M[\[CapitalXi]][r]^2M[L1][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[L1]}]+2Sum[\[CurlyEpsilon][\[CapitalXi],L1][s,r,t]\[Kappa][\[CapitalXi]][s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][t]/(M[L1][r]^2M[\[CapitalXi]][s]^2M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CapitalXi]]},{t,1,n[L1]}]+4Sum[Re[Conjugate[g1[\[CapitalXi]1,L1][r,s]]Conjugate[\[Gamma][L1][s]]\[Kappa][\[CapitalXi]1][r]]/(M[L1][s]^2M[\[CapitalXi]1][r]^2),{s,1,n[L1]},{r,1,n[\[CapitalXi]1]}]-4Sum[Re[g2[\[CapitalXi]1,L1][r,s]Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Gamma][L1][s]]/(M[L1][s]^2M[\[CapitalXi]1][r]^2),{s,1,n[L1]},{r,1,n[\[CapitalXi]1]}]-4Sum[Re[Conjugate[\[CurlyEpsilon][\[CapitalXi]1,L1][r,s,t]]Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Gamma][L1][s]\[Gamma][L1][t]]/(M[L1][s]^2M[\[CapitalXi]1][r]^2M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CapitalXi]1]},{t,1,n[L1]}])/(Z\[Phi]^2);
Subscript[wC, \[Phi]Sq]:=(-Sum[(\[Kappa][S][r])^2/(2M[S][r]^4),{r,1,n[S]}]+Sum[(\[Kappa][\[CapitalXi]][r])^2/(2M[\[CapitalXi]][r]^4),{r,1,n[\[CapitalXi]]}]+2Sum[Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Kappa][\[CapitalXi]1][r]/(M[\[CapitalXi]1][r]^4),{r,1,n[\[CapitalXi]1]}]-Sum[Re[gHat[B][\[Phi]][r]gHat[B][\[Phi]][r]]/(2M[B][r]^2),{r,1,n[B]}]-Sum[Re[gHat[W][\[Phi]][r]gHat[W][\[Phi]][r]]/(8M[W][r]^2),{r,1,n[W]}]-Sum[Conjugate[gHat[B1][\[Phi]][r]]gHat[B1][\[Phi]][r]/(2M[B1][r]^2),{r,1,n[B1]}]-Sum[Conjugate[gHat[W][\[Phi]][r]]gHat[W][\[Phi]][r]/(4M[W][r]^2),{r,1,n[W]}]-Sum[Conjugate[gHat[W1][\[Phi]][r]]gHat[W1][\[Phi]][r]/(8M[W1][r]^2),{r,1,n[W1]}]+Subscript[g1, SM] Sum[g[L1][B][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(4M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]+3Subscript[g2, SM] Sum[g[L1][W][s,r]Conjugate[\[Gamma][L1][s]]\[Gamma][L1][r]/(4M[L1][r]^2*M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]-Sum[h1[L1][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(2M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]+Sum[Re[h3[L1][r,s]Conjugate[\[Gamma][L1][r]]Conjugate[\[Gamma][L1][s]]]/(M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]+Sum[Im[gHat[B][\[Phi]][r]]\[Delta][B,S][r,s]\[Kappa][S][s]/(M[B][r]^2M[S][s]^2),{r,1,n[B]},{s,1,n[S]}]+Sum[\[Delta][B,S][r,t]\[Delta][B,S][r,s]\[Kappa][S][s]\[Kappa][S][t]/(2M[B][r]^2M[S][s]^2 M[S][t]^2),{r,1,n[B]},{s,1,n[S]},{t,1,n[S]}]-Sum[Im[gHat[W][\[Phi]][r]]\[Delta][W,\[CapitalXi]][r,s]\[Kappa][\[CapitalXi]][s]/(2M[W][r]^2M[\[CapitalXi]][s]^2),{r,1,n[W]},{s,1,n[\[CapitalXi]]}]-Sum[\[Delta][W,\[CapitalXi]][t,s]\[Delta][W,\[CapitalXi]][t,r]\[Kappa][\[CapitalXi]][r]\[Kappa][\[CapitalXi]][s]/(2M[\[CapitalXi]][r]^2M[\[CapitalXi]][s]^2M[W][t]^2),{r,1,n[\[CapitalXi]]},{s,1,n[\[CapitalXi]]},{t,1,n[W]}]-Sum[Im[gHat[W1][\[Phi]][r]\[Delta][W1,\[CapitalXi]1][r,s]\[Kappa][\[CapitalXi]1][s]]/(M[W1][r]^2M[\[CapitalXi]1][s]^2),{r,1,n[W1]},{s,1,n[\[CapitalXi]1]}]-2Sum[Conjugate[\[Delta][W1,\[CapitalXi]1][s,t]]\[Delta][W1,\[CapitalXi]1][s,r]\[Kappa][\[CapitalXi]1][r]Conjugate[\[Kappa][\[CapitalXi]1][t]]/(M[\[CapitalXi]1][r]^2M[W1][s]^2M[\[CapitalXi]1][t]^2),{r,1,n[\[CapitalXi]1]},{s,1,n[W1]},{t,1,n[\[CapitalXi]1]}]-Sum[Re[Conjugate[g1[S,L1][r,s]]Conjugate[\[Gamma][L1][s]]]\[Kappa][S][r]/(M[S][r]^2M[L1][s]^2),{r,1,n[S]},{s,1,n[L1]}]+Sum[Re[Conjugate[g2[S,L1][r,s]]Conjugate[\[Gamma][L1][s]]]\[Kappa][S][r]/(M[S][r]^2M[L1][s]^2),{r,1,n[S]},{s,1,n[L1]}]+Sum[\[CurlyEpsilon][S,L1][r,t,s]\[Kappa][S][r]Conjugate[\[Gamma][L1][t]]\[Gamma][L1][s]/(2M[S][r]^2M[L1][s]^2M[L1][t]^2),{r,1,n[S]},{s,1,n[L1]},{t,1,n[L1]}]+Sum[Re[Conjugate[g1[\[CapitalXi],L1][r,s]]Conjugate[\[Gamma][L1][s]]]\[Kappa][\[CapitalXi]][r]/(M[\[CapitalXi]][r]^2M[L1][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[L1]}]-Sum[Re[g1[\[CapitalXi],L1][r,s]\[Gamma][L1][s]]\[Kappa][\[CapitalXi]][r]/(M[\[CapitalXi]][r]^2M[L1][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[L1]}]-Sum[\[CurlyEpsilon][\[CapitalXi],L1][s,r,t]\[Kappa][\[CapitalXi]][s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][t]/(2M[\[CapitalXi]][s]^2M[L1][r]^2M[L1][t]^2),{s,1,n[\[CapitalXi]]},{r,1,n[L1]},{t,1,n[L1]}]+2Sum[Re[Conjugate[g1[\[CapitalXi]1,L1][r,s]]Conjugate[\[Gamma][L1][s]]\[Kappa][\[CapitalXi]1][r]]/(M[\[CapitalXi]1][r]^2M[L1][s]^2),{s,1,n[L1]},{r,1,n[\[CapitalXi]1]}]-2Sum[Re[g2[\[CapitalXi]1,L1][r,s]\[Gamma][L1][s]Conjugate[\[Kappa][\[CapitalXi]1][r]]]/(M[\[CapitalXi]1][r]^2M[L1][s]^2),{s,1,n[L1]},{r,1,n[\[CapitalXi]1]}]-2Sum[Re[Conjugate[\[CurlyEpsilon][\[CapitalXi],L1][r,s,t]]Conjugate[\[Kappa][\[CapitalXi]1][r]]Conjugate[\[Gamma][L1][t]]\[Gamma][L1][s]]/(M[\[CapitalXi]1][r]^2M[L1][s]^2M[L1][t]^2),{r,1,n[\[CapitalXi]1]},{s,1,n[L1]},{t,1,n[L1]}])/(Z\[Phi]^2);
Subscript[wC, \[Phi]B]:=(-Subscript[g1, SM]^2 Sum[Conjugate[\[Gamma][L1][r]]\[Gamma][L1][r]/(8M[L1][r]^4),{r,1,n[L1]}]-Subscript[g1, SM] Sum[g[L1][B][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(4M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}])/Z\[Phi];
Subscript[wC, \[Phi]Btil]:=(-Subscript[g1, SM] Sum[g[L1][Btil][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(4M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}])/Z\[Phi];
Subscript[wC, \[Phi]W]:=(-Subscript[g2, SM]^2 Sum[Conjugate[\[Gamma][L1][r]]\[Gamma][L1][r]/(8M[L1][r]^4),{r,1,n[L1]}]-Subscript[g2, SM] Sum[g[L1][W][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(4M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}])/Z\[Phi];
Subscript[wC, \[Phi]Wtil]:=(-Subscript[g2, SM] Sum[g[L1][Wtil][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(4M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}])/Z\[Phi];
Subscript[wC, \[Phi]WB]:=(-Subscript[g1, SM] Subscript[g2, SM] Sum[Conjugate[\[Gamma][L1][r]]\[Gamma][L1][r]/(4M[L1][r]^4),{r,1,n[L1]}]-Subscript[g2, SM] Sum[g[L1][B][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(4M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]-Subscript[g1, SM] Sum[g[L1][Wtil][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(4M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}])/Z\[Phi];
Subscript[wC, \[Phi]WBtil]:=(-Subscript[g2, SM] Sum[g[L1][Btil][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(4M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]-Subscript[g1, SM] Sum[g[L1][Wtil][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(4M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}])/Z\[Phi];
Subscript[wC, \[Phi]G]=0;
Subscript[wC, \[Phi]Gtil]=0;
a\[Psi]\[Phi]:=Sum[(\[Kappa][\[CapitalXi]][r])^2/(M[\[CapitalXi]][r]^4),{r,1,n[\[CapitalXi]]}]+2Sum[(Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Kappa][\[CapitalXi]1][r])/(M[\[CapitalXi]1][r]^4),{r,1,n[\[CapitalXi]1]}]-I*Sum[Im[(gHat[B][\[Phi]][r])^2]/(2M[B][r]^2),{r,1,n[B]}]-Sum[Conjugate[(gHat[B1][\[Phi]][r])](gHat[B1][\[Phi]][r])/(2M[B1][r]^2),{r,1,n[B1]}]-I*Sum[Im[(gHat[W][\[Phi]][r])^2]/(8M[W][r]^2),{r,1,n[W]}]-Sum[Conjugate[(gHat[W][\[Phi]][r])](gHat[W][\[Phi]][r])/(4M[W][r]^2),{r,1,n[W]}]-Sum[Conjugate[(gHat[W1][\[Phi]][r])](gHat[W1][\[Phi]][r])/(8M[W1][r]^2),{r,1,n[W1]}]+Subscript[g2, SM] Sum[g[L1][W][s,r]Conjugate[\[Gamma][L1][s]]\[Gamma][L1][r]/(2M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]-Sum[h1[L1][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(2M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]-I*Sum[Im[h3[L1][r,s]Conjugate[\[Gamma][L1][r]]Conjugate[\[Gamma][L1][s]]]/(M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]-I Sum[Re[gHat[B][\[Phi]][r]]\[Delta][B,S][r,s]\[Kappa][S][s]/(M[B][r]^2M[S][s]^2),{r,1,n[B]},{s,1,n[S]}]+I*Sum[(gHat[W][\[Phi]][r]-3Conjugate[gHat[W][\[Phi]][r]])\[Delta][W,\[CapitalXi]][r,s]\[Kappa][\[CapitalXi]][s]/(4M[W][r]^2M[\[CapitalXi]][s]^2),{r,1,n[W]},{s,1,n[\[CapitalXi]]}]-Sum[\[Delta][W,\[CapitalXi]][t,s]\[Delta][W,\[CapitalXi]][t,r]\[Kappa][\[CapitalXi]][r]\[Kappa][\[CapitalXi]][s]/(M[\[CapitalXi]][r]^2M[\[CapitalXi]][s]^2M[W][t]^2),{r,1,n[\[CapitalXi]]},{s,1,n[\[CapitalXi]]},{t,1,n[W]}]-Sum[Im[Conjugate[gHat[W1][\[Phi]][r]]\[Delta][W1,\[CapitalXi]1][r,s]\[Kappa][\[CapitalXi]1][s]]/(M[\[CapitalXi]1][s]^2M[W1][r]^2),{s,1,n[\[CapitalXi]1]},{r,1,n[W1]}]-2Sum[Conjugate[\[Delta][W1,\[CapitalXi]1][s,t]]\[Delta][W1,\[CapitalXi]1][s,r]\[Kappa][\[CapitalXi]1][r]Conjugate[\[Kappa][\[CapitalXi]1][t]]/(M[\[CapitalXi]1][r]^2M[W1][s]^2M[\[CapitalXi]1][t]^2),{r,1,n[\[CapitalXi]1]},{s,1,n[W1]},{t,1,n[\[CapitalXi]1]}]+I*Sum[Im[Conjugate[g1[S,L1][r,s]]Conjugate[\[Gamma][L1][s]]]\[Kappa][S][r]/(M[S][r]^2M[L1][s]^2),{r,1,n[S]},{s,1,n[L1]}]+Sum[Re[Conjugate[g2[S,L1][r,s]]Conjugate[\[Gamma][L1][s]]]\[Kappa][S][r]/(M[S][r]^2M[L1][s]^2),{r,1,n[S]},{s,1,n[L1]}]+Sum[\[CurlyEpsilon][S,L1][r,t,s]\[Kappa][S][r]Conjugate[\[Gamma][L1][t]]\[Gamma][L1][s]/(2M[S][r]^2M[L1][s]^2M[L1][t]^2),{r,1,n[S]},{s,1,n[L1]},{t,1,n[L1]}]+Sum[\[Delta][L1,\[CurlyPhi]][s,r]Conjugate[\[Gamma][L1][s]]\[Lambda][\[CurlyPhi]][r]/(M[\[CurlyPhi]][r]^2M[L1][s]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[L1]}]+2Sum[\[Lambda]Hat[\[Phi]]Conjugate[\[Delta][L1,\[CurlyPhi]][t,s]]\[Gamma][L1][t]\[Delta][L1,\[CurlyPhi]][r,s]Conjugate[\[Gamma][L1][r]]/(M[L1][r]^2M[\[CurlyPhi]][s]^2M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CurlyPhi]]},{t,1,n[L1]}]+Sum[(g1[\[CapitalXi],L1][r,s]\[Gamma][L1][s]+3Conjugate[g1[\[CapitalXi],L1][r,s]]Conjugate[\[Gamma][L1][s]])\[Kappa][\[CapitalXi]][r]/(2M[\[CapitalXi]][r]^2*M[L1][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[L1]}]-Sum[Re[g2[\[CapitalXi],L1][r,s]\[Gamma][L1][s]]\[Kappa][\[CapitalXi]][r],{r,1,n[\[CapitalXi]]},{s,1,n[L1]}]-Sum[\[CurlyEpsilon][\[CapitalXi],L1][s,r,t]\[Kappa][\[CapitalXi]][s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][t]/(2M[L1][r]^2M[\[CapitalXi]][s]^2M[L1][t]^2),{r,1,n[L1]},{s,1,n[\[CapitalXi]]},{t,1,n[L1]}]+2Sum[Re[Conjugate[g1[\[CapitalXi]1,L1][r,s]]Conjugate[\[Gamma][L1][s]]\[Kappa][\[CapitalXi]1][r]/(M[L1][s]^2M[\[CapitalXi]1][r]^2)],{s,1,n[L1]},{r,1,n[\[CapitalXi]1]}]-2I*Sum[Im[(g[\[CapitalXi]1,L1]^(2))[r,s]Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Gamma][L1][s]]/(M[L1][s]^2M[\[CapitalXi]1][r]^2),{s,1,n[L1]},{r,1,n[\[CapitalXi]1]}]-2I*Sum[Im[Conjugate[\[CurlyEpsilon][\[CapitalXi]1,L1][r,s,t]]Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Gamma][L1][t]\[Gamma][L1][s]]/(M[L1][s]^2M[L1][t]^2M[\[CapitalXi]1][r]^2),{s,1,n[L1]},{t,1,n[L1]},{r,1,n[\[CapitalXi]1]}]-Sum[\[Delta][L1,\[CurlyPhi]][t,r]Conjugate[\[Gamma][L1][t]]\[Kappa][S,\[CurlyPhi]][s,r]\[Kappa][S][s]/(M[\[CurlyPhi]][r]^2M[S][s]^2M[L1][t]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[S]},{t,1,n[L1]}]-Sum[\[Delta][L1,\[CurlyPhi]][r,t]Conjugate[\[Gamma][L1][r]]\[Kappa][\[CapitalXi],\[CurlyPhi]][s,t]\[Kappa][\[CapitalXi]][s]/(M[L1][r]^2M[\[CapitalXi]][s]^2M[\[CurlyPhi]][t]^2),{r,1,n[L1]},{s,1,n[\[CapitalXi]]},{t,1,n[\[CurlyPhi]]}]-2Sum[\[Delta][L1,\[CurlyPhi]][s,r]Conjugate[\[Gamma][L1][s]]Conjugate[\[Kappa][\[CapitalXi]1,\[CurlyPhi]][t,r]]\[Kappa][\[CapitalXi]1][t]/(M[\[CurlyPhi]][r]^2M[L1][s]^2M[\[CapitalXi]1][t]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[L1]},{t,1,n[\[CapitalXi]1]}];
b[e][i_,j_]:=Sum[\[Lambda][\[CurlyPhi]][r]Conjugate[y[\[CurlyPhi]][e][r,j,i]]/(M[\[CurlyPhi]][r]^2),{r,1,n[\[CurlyPhi]]}]+Sum[Conjugate[yHat[e][j,k]]\[Lambda][Ef][r,k]Conjugate[\[Lambda][Ef][r,i]]/(2M[Ef][r]^2),{r,1,n[Ef]},{k,1,3}]+Sum[Conjugate[yHat[e][k,i]]\[Lambda][\[CapitalDelta]1][r,j]Conjugate[\[Lambda][\[CapitalDelta]1][r,k]]/(2M[\[CapitalDelta]1][r]^2),{r,1,n[\[CapitalDelta]1]},{k,1,3}]+Sum[Conjugate[yHat[e][k,i]]\[Lambda][\[CapitalDelta]3][r,j]Conjugate[\[Lambda][\[CapitalDelta]3][r,k]]/(2M[\[CapitalDelta]3][r]^2),{r,1,n[\[CapitalDelta]3]},{k,1,3}]+Sum[Conjugate[yHat[e][j,k]]\[Lambda][\[CapitalSigma]][r,k]Conjugate[\[Lambda][\[CapitalSigma]][r,i]]/(4M[\[CapitalSigma]][r]^2),{r,1,n[\[CapitalSigma]]},{k,1,3}]+Sum[Conjugate[yHat[e][j,k]]\[Lambda][\[CapitalSigma]1][r,k]Conjugate[\[Lambda][\[CapitalSigma]1][r,i]]/(8M[\[CapitalSigma]1][r]^2),{r,1,n[\[CapitalSigma]1]},{k,1,3}]+I*Sum[Conjugate[yHat[e][j,k]]Im[gHat[B][\[Phi]][r]]g[B][L][r,i,k]/(M[B][r]^2),{r,1,n[B]},{k,1,3}]-I*Sum[Conjugate[yHat[e][k,i]]Im[gHat[B][\[Phi]][r]]g[B][e][r,k,j]/(M[B][r]^2),{r,1,n[B]},{k,1,3}]+I*Sum[Conjugate[yHat[e][j,k]]Im[gHat[W][\[Phi]][r]]g[W][L][r,i,k]/(4M[W][r]^2),{r,1,n[W]},{k,1,3}];
c[e][i_,j_]:=-Sum[\[Kappa][S,\[CurlyPhi]][r,s]\[Kappa][S][r]Conjugate[y[\[CurlyPhi]][e][s,j,i]]/(M[S][r]^2M[\[CurlyPhi]][s]^2),{r,1,n[S]},{s,1,n[\[CurlyPhi]]}]-Sum[\[Kappa][\[CapitalXi],\[CurlyPhi]][s,r]\[Kappa][\[CapitalXi]][s]Conjugate[y[\[CurlyPhi]][e][r,j,i]]/(M[\[CurlyPhi]][r]^2M[\[CapitalXi]][s]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[\[CapitalXi]]}]-2Sum[Conjugate[\[Kappa][\[CapitalXi]1,\[CurlyPhi]][s,r]]\[Kappa][\[CapitalXi]1][s]Conjugate[y[\[CurlyPhi]][e][r,j,i]]/(M[\[CurlyPhi]][r]^2M[\[CapitalXi]1][s]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[\[CapitalXi]1]}]-Sum[\[Lambda][Ef,\[CapitalDelta]1][r,s]Conjugate[\[Lambda][Ef][r,i]]\[Lambda][\[CapitalDelta]1][s,j]/(M[Ef][r]M[\[CapitalDelta]1][s]),{r,1,n[Ef]},{s,1,n[\[CapitalDelta]1]}]-Sum[\[Lambda][Ef,\[CapitalDelta]3][s,r]Conjugate[\[Lambda][Ef][s,i]]\[Lambda][\[CapitalDelta]3][r,j]/(M[Ef][s]M[\[CapitalDelta]3][r]),{s,1,n[Ef]},{r,1,n[\[CapitalDelta]3]}]-Sum[Conjugate[\[Lambda][\[CapitalSigma]][s,i]]\[Lambda][\[CapitalSigma],\[CapitalDelta]1][s,r]\[Lambda][\[CapitalDelta]1][r,j]/(2M[\[CapitalDelta]1][r]M[\[CapitalSigma]][s]),{r,1,n[\[CapitalDelta]1]},{s,1,n[\[CapitalSigma]]}]-Sum[Conjugate[\[Lambda][\[CapitalSigma]1][r,i]]\[Lambda][\[CapitalSigma]1,\[CapitalDelta]1][r,s]\[Lambda][\[CapitalDelta]1][s,j]/(4M[\[CapitalDelta]1][s]M[\[CapitalSigma]1][r]),{s,1,n[\[CapitalDelta]1]},{r,1,n[\[CapitalSigma]1]}]+Sum[Conjugate[\[Lambda][\[CapitalSigma]1][s,i]]\[Lambda][\[CapitalSigma]1,\[CapitalDelta]3][s,r]\[Lambda][\[CapitalDelta]3][r,j]/(4M[\[CapitalDelta]3][r]M[\[CapitalSigma]1][s]),{s,1,n[\[CapitalDelta]3]},{r,1,n[\[CapitalSigma]1]}]-Sum[w[S,Ef][r,s,j]\[Kappa][S][r]Conjugate[\[Lambda][Ef][s,i]]/(M[S][r]^2M[Ef][s]),{r,1,n[S]},{s,1,n[Ef]}]-Sum[w[S,\[CapitalDelta]1][r,s,i]\[Kappa][S][r]\[Lambda][\[CapitalDelta]1][s,j]/(M[S][r]^2M[\[CapitalDelta]1][s]),{r,1,n[S]},{s,1,n[\[CapitalDelta]1]}]-Sum[Conjugate[w[\[CapitalXi],\[CapitalDelta]3][r,s,i]]\[Kappa][\[CapitalXi]][r]\[Lambda][\[CapitalDelta]1][s,j]/(M[\[CapitalXi]][r]^2M[\[CapitalDelta]1][s]),{r,1,n[\[CapitalXi]]},{s,1,n[\[CapitalDelta]1]}]-Sum[w[\[CapitalXi],\[CapitalSigma]1][s,r,j]\[Kappa][\[CapitalXi]][s]Conjugate[\[Lambda][\[CapitalSigma]1][r,i]]/(2M[\[CapitalSigma]1][r]M[\[CapitalXi]][s]^2),{r,1,n[\[CapitalSigma]1]},{s,1,n[\[CapitalXi]]}]-2Sum[Conjugate[w[\[CapitalXi]1,\[CapitalDelta]3][r,s,i]]\[Kappa][\[CapitalXi]1][r]\[Lambda][\[CapitalDelta]3][s,j]/(M[\[CapitalXi]1][r]^2M[\[CapitalDelta]3][s]),{r,1,n[\[CapitalXi]1]},{s,1,n[\[CapitalDelta]3]}]-Sum[Conjugate[\[Lambda][\[CapitalSigma]][s,i]]Conjugate[w[\[CapitalXi]1,\[CapitalSigma]][r,s,j]]\[Kappa][\[CapitalXi]1][r]/(M[\[CapitalXi]1][r]^2M[\[CapitalSigma]][s]),{r,1,n[\[CapitalXi]1]},{s,1,n[\[CapitalSigma]]}]+I*Sum[Conjugate[yHat[e][j,k]]z[Ef,L1][r,s,k]Conjugate[\[Lambda][Ef][r,i]]Conjugate[\[Gamma][L1][s]]/(2M[Ef][r]M[L1][s]^2),{r,1,n[Ef]},{s,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[e][j,k]]Conjugate[z[Ef,L1][r,s,i]]\[Lambda][Ef][r,k]\[Gamma][L1][s]/(2M[Ef][r]M[L1][s]^2),{r,1,n[Ef]},{s,1,n[L1]},{k,1,3}]-I*Sum[Conjugate[yHat[e][k,i]]Conjugate[z[\[CapitalDelta]1,L1][r,s,k]]\[Lambda][\[CapitalDelta]1][r,j]Conjugate[\[Gamma][L1][s]]/(2M[\[CapitalDelta]1][r]M[L1][s]^2),{r,1,n[\[CapitalDelta]1]},{s,1,n[L1]},{k,1,3}]-I*Sum[Conjugate[yHat[e][k,i]]z[\[CapitalDelta]1,L1][r,s,j]Conjugate[\[Lambda][\[CapitalDelta]1][r,k]]Conjugate[\[Gamma][L1][s]]/(2M[\[CapitalDelta]1][r]M[L1][s]^2),{r,1,n[\[CapitalDelta]1]},{s,1,n[L1]},{k,1,3}]-I*Sum[Conjugate[yHat[e][k,i]]Conjugate[z[\[CapitalDelta]3,L1][s,r,k]]\[Lambda][\[CapitalDelta]3][s,j]\[Gamma][L1][r]/(2M[\[CapitalDelta]3][s]M[L1][r]^2),{r,1,n[L1]},{s,1,n[\[CapitalDelta]3]},{k,1,3}]-I*Sum[Conjugate[yHat[e][k,i]]z[\[CapitalDelta]3,L1][s,r,j]Conjugate[\[Lambda][\[CapitalDelta]3][s,k]]Conjugate[\[Gamma][L1][r]]/(2M[\[CapitalDelta]3][s]M[L1][r]^2),{r,1,n[L1]},{s,1,n[\[CapitalDelta]3]},{k,1,3}]+I*Sum[Conjugate[yHat[e][j,k]]z[\[CapitalSigma],L1][r,s,k]Conjugate[\[Lambda][\[CapitalSigma]][r,i]]\[Gamma][L1][s]/(2M[\[CapitalSigma]][r]M[L1][s]^2),{r,1,n[\[CapitalSigma]]},{s,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[e][j,k]]Conjugate[z[\[CapitalSigma],L1][s,r,i]]\[Lambda][\[CapitalSigma]][s,k]Conjugate[\[Gamma][L1][r]]/(2M[\[CapitalSigma]][s]M[L1][r]^2),{r,1,n[L1]},{s,1,n[\[CapitalSigma]]},{k,1,3}]+I*Sum[Conjugate[yHat[e][j,k]]z[\[CapitalSigma]1,L1][s,r,k]Conjugate[\[Lambda][\[CapitalSigma]1][s,i]]Conjugate[\[Gamma][L1][r]]/(4M[\[CapitalSigma]1][s]M[L1][r]^2),{s,1,n[\[CapitalSigma]1]},{r,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[e][j,k]]Conjugate[z[\[CapitalSigma]1,L1][r,s,k]]\[Lambda][\[CapitalSigma]1][r,k]\[Gamma][L1][s]/(4M[\[CapitalSigma]1][r]M[L1][s]^2),{r,1,n[\[CapitalSigma]1]},{s,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[e][j,k]]g[B][L][r,i,k]\[Delta][B,S][r,s]\[Kappa][S][s]/(M[B][r]^2M[S][s]^2),{r,1,n[B]},{s,1,n[S]},{k,1,3}]-I*Sum[Conjugate[yHat[e][k,i]]g[B][e][r,k,j]\[Delta][B,S][r,s]\[Kappa][S][s]/(M[B][r]^2M[S][s]^2),{r,1,n[B]},{s,1,n[S]},{k,1,3}]+I*Sum[Conjugate[yHat[e][j,k]]g[W][L][s,i,k]\[Delta][W,\[CapitalXi]][s,r]\[Kappa][\[CapitalXi]][r]/(2M[\[CapitalXi]][r]^2M[W][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[W]},{k,1,3}]+
2\[Lambda]Hat[\[Phi]]Sum[Conjugate[\[Delta][L1,\[CurlyPhi]][s,r]]\[Gamma][L1][s]Conjugate[y[\[CurlyPhi]][e][r,j,i]]/(M[\[CurlyPhi]][r]^2 M[L1][s]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[L1]}];
b[d][i_,j_]:=Sum[\[Lambda][\[CurlyPhi]][r]Conjugate[y[\[CurlyPhi]][d][r,j,i]]/(M[\[CurlyPhi]][r]^2),{r,1,n[\[CurlyPhi]]}]+Sum[Conjugate[yHat[d][j,k]]\[Lambda][Df][r,k]Conjugate[\[Lambda][Df][r,i]]/(2M[Df][r]^2),{r,1,n[Df]},{k,1,3}]+Sum[Conjugate[yHat[d][k,i]]\[Lambda][Q5][r,j]Conjugate[\[Lambda][Q5][r,k]]/(2M[Q5][r]^2),{r,1,n[Q5]},{k,1,3}]+Sum[Conjugate[yHat[d][k,i]]\[Lambda][Q1][d][r,j] Conjugate[\[Lambda][Q1][d][r,k]]/(2M[Q1][r]^2),{r,1,n[Q1]},{k,1,3}]+Sum[Conjugate[yHat[d][j,k]]\[Lambda][T1][r,k]Conjugate[\[Lambda][T1][r,i]]/(8M[T1][r]^2),{r,1,n[T1]},{k,1,3}]+Sum[Conjugate[yHat[d][j,k]]\[Lambda][T2][r,k]Conjugate[\[Lambda][T2][r,i]]/(4M[T2][r]^2),{r,1,n[T2]},{k,1,3}]+I*Sum[Conjugate[yHat[d][j,k]]Im[gHat[B][\[Phi]][r]]g[B][q][r,i,k]/(M[B][r]^2),{r,1,n[B]},{k,1,3}]-I*Sum[Conjugate[yHat[d][k,i]]Im[gHat[B][\[Phi]][r]]g[B][d][r,k,j]/(M[B][r]^2),{r,1,n[B]},{k,1,3}]+I*Sum[Conjugate[yHat[d][j,k]]Im[gHat[W][\[Phi]][r]]g[W][q][r,i,k]/(4M[W][r]^2),{r,1,n[W]},{k,1,3}];
c[d][i_,j_]:=-Sum[\[Kappa][S,\[CurlyPhi]][r,s]*\[Kappa][S][r]*Conjugate[y[\[CurlyPhi]][d][s,j,i]]/(M[S][r]^2M[\[CurlyPhi]][s]^2),{r,1,n[S]},{s,1,n[\[CurlyPhi]]}]-Sum[\[Kappa][\[CapitalXi],\[CurlyPhi]][r,s]\[Kappa][\[CapitalXi]][r]Conjugate[y[\[CurlyPhi]][d][s,j,i]]/(M[\[CapitalXi]][r]^2M[\[CurlyPhi]][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[\[CurlyPhi]]}]-2Sum[Conjugate[\[Kappa][\[CapitalXi]1,\[CurlyPhi]][s,r]]\[Kappa][\[CapitalXi]1][s]Conjugate[y[\[CurlyPhi]][d][r,j,i]]/(M[\[CapitalXi]1][s]^2M[\[CurlyPhi]][r]^2),{s,1,n[\[CapitalXi]1]},{r,1,n[\[CurlyPhi]]}]-Sum[\[Lambda][Df,Q1][s,r]Conjugate[\[Lambda][Df][s,i]]\[Lambda][Q1][d][r,j]/(M[Q1][r]M[Df][s]),{r,1,n[Q1]},{s,1,n[Df]}]-Sum[\[Lambda][Df,Q5][r,s]Conjugate[\[Lambda][Df][r,i]]\[Lambda][Q5][s,j]/(M[Q5][r]M[Df][s]),{r,1,n[Q5]},{s,1,n[Df]}]-Sum[\[Lambda][T1,Q1][r,s]Conjugate[\[Lambda][T1][r,i]]\[Lambda][Q1][d][s,j]/(4M[T1][r]M[Q1][s]),{r,1,n[T1]},{s,1,n[Q1]}]-Sum[\[Lambda][T2,Q1][s,r]Conjugate[\[Lambda][T2][s,i]]\[Lambda][Q1][d][r,j]/(2M[Q1][r]M[T2][s]),{r,1,n[Q1]},{s,1,n[T2]}]+Sum[\[Lambda][T1,Q5][r,s]Conjugate[\[Lambda][T1][r,i]]\[Lambda][Q5][s,j]/(4M[T1][r]M[Q5][s]),{r,1,n[T1]},{s,1,n[Q5]}]-Sum[w[S,Df][r,s,j]\[Kappa][S][r]Conjugate[\[Lambda][Df][s,i]]/(M[S][r]^2M[Df][s]),{r,1,n[S]},{s,1,n[Df]}]-Sum[Conjugate[w[S,Q1][r,s,i]]\[Kappa][S][r]Conjugate[\[Lambda][Q1][d][s,j]]/(M[S][r]^2M[Q1][s]),{r,1,n[S]},{s,1,n[Q1]}]-Sum[Conjugate[w[\[CapitalXi],Q1][s,r,i]]\[Kappa][\[CapitalXi]][s]\[Lambda][Q1][d][r,j]/(M[\[CapitalXi]][s]^2M[Q1][r]),{s,1,n[\[CapitalXi]]},{r,1,n[Q1]}]-Sum[w[\[CapitalXi],T1][r,s,j]\[Kappa][\[CapitalXi]][r]Conjugate[\[Lambda][T1][s,i]]/(2M[\[CapitalXi]][r]^2M[T1][s]),{r,1,n[\[CapitalXi]]},{s,1,n[T1]}]-2Sum[Conjugate[w[\[CapitalXi]1,Q5][r,s,i]]\[Kappa][\[CapitalXi]1][r]\[Lambda][Q5][s,j]/(M[\[CapitalXi]1][r]^2M[Q5][s]),{r,1,n[\[CapitalXi]1]},{s,1,n[Q5]}]-Sum[w[\[CapitalXi]1,T2][r,s,j]\[Kappa][\[CapitalXi]1][r]Conjugate[\[Lambda][T2][s,i]]/(M[\[CapitalXi]1][r]^2M[T2][s]),{r,1,n[\[CapitalXi]1]},{s,1,n[T2]}]+I*Sum[Conjugate[yHat[d][j,k]]z[Df,L1][r,s,k]Conjugate[\[Lambda][Df][r,i]]Conjugate[\[Gamma][L1][s]]/(2M[Df][r]M[L1][s]^2),{r,1,n[Df]},{s,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[d][j,k]]Conjugate[z[Df,L1][r,s,k]]\[Lambda][Df][r,k]\[Gamma][L1][s]/(2M[Df][r]M[L1][s]^2),{r,1,n[Df]},{s,1,n[L1]},{k,1,3}]-I*Sum[Conjugate[yHat[d][k,i]]z[Q1,L1][d][r,s,j]Conjugate[\[Lambda][Q1][d][r,k]]\[Gamma][L1][s]/(2M[Q1][r]M[L1][s]^2),{r,1,n[Q1]},{s,1,n[L1]},{k,1,3}]-I*Sum[Conjugate[yHat[d][k,i]]Conjugate[z[Q1,L1][d][r,s,k]]\[Lambda][Q1][d][r,j]Conjugate[\[Gamma][L1][s]]/(2M[Q1][r]M[L1][s]^2),{r,1,n[Q1]},{s,1,n[L1]},{k,1,3}]-I*Sum[Conjugate[yHat[d][k,i]]Conjugate[z[Q5,L1][r,s,k]]\[Lambda][Q5][r,j]\[Gamma][L1][s]/(2M[Q5][r]M[L1][s]^2),{r,1,n[Q5]},{s,1,n[L1]},{k,1,3}]-I*Sum[Conjugate[yHat[d][j,k]]z[Q5,L1][r,s,j]Conjugate[\[Lambda][Q5][r,k]]Conjugate[\[Gamma][L1][s]]/(2M[Q5][r]M[L1][s]^2),{r,1,n[Q5]},{s,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[d][j,k]]z[T2,L1][r,s,k]Conjugate[\[Lambda][T2][r,i]]\[Gamma][L1][s]/(2M[T2][r]M[L1][s]^2),{r,1,n[T2]},{s,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[d][j,k]]Conjugate[z[T2,L1][s,r,i]]\[Lambda][T2][s,k]Conjugate[\[Gamma][L1][r]]/(2M[T2][s]M[L1][r]^2),{s,1,n[T2]},{r,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[d][j,k]]z[T1,L1][r,s,k]Conjugate[\[Lambda][T1][r,i]]Conjugate[\[Gamma][L1][s]]/(4M[T1][r]M[L1][s]^2),{r,1,n[T1]},{s,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[d][j,k]]Conjugate[z[T1,L1][s,r,i]]\[Lambda][T1][s,k]\[Gamma][L1][r]/(2M[T1][s]M[L1][r]^2),{s,1,n[T1]},{r,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[d][j,k]]\[Delta][B,S][r,s]g[B][q][r,i,k]\[Kappa][S][s]/(M[B][r]^2M[S][s]^2),{r,1,n[B]},{s,1,n[S]},{k,1,3}]-I*Sum[Conjugate[yHat[d][k,i]]\[Delta][B,S][r,s]g[B][d][r,k,j]\[Kappa][S][s]/(M[B][r]^2M[S][s]^2),{r,1,n[B]},{s,1,n[S]},{k,1,3}]+I*Sum[Conjugate[yHat[d][j,k]]\[Delta][W,\[CapitalXi]][s,r]g[W][q][s,i,k]\[Kappa][\[CapitalXi]][r]/(2 M[\[CapitalXi]][r]^2M[W][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[W]},{k,1,3}]+2\[Lambda]Hat[\[Phi]]Sum[Conjugate[\[Delta][L1,\[CurlyPhi]][s,r]]\[Gamma][L1][s]Conjugate[y[\[CurlyPhi]][d][r,j,i]]/(M[\[CurlyPhi]][r]^2M[L1][s]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[L1]}];
b[u][i_,j_]:=-Sum[Conjugate[\[Lambda][\[CurlyPhi]][r]]y[\[CurlyPhi]][u][r,i,j]/(M[\[CurlyPhi]][r]^2),{r,1,n[\[CurlyPhi]]}]+Sum[Conjugate[yHat[u][j,k]]\[Lambda][U][r,k]Conjugate[\[Lambda][U][r,i]]/(2M[U][r]^2),{r,1,n[U]},{k,1,3}]+Sum[Conjugate[yHat[u][k,i]]\[Lambda][Q1][u][r,j]Conjugate[\[Lambda][Q1][u][r,k]]/(2M[Q1][r]^2),{r,1,n[Q1]},{k,1,3}]+Sum[Conjugate[yHat[u][k,i]]\[Lambda][Q7][r,j]Conjugate[\[Lambda][Q7][r,k]]/(2M[Q7][r]^2),{r,1,n[Q7]},{k,1,3}]+Sum[Conjugate[yHat[u][j,k]]\[Lambda][T1][r,k]Conjugate[\[Lambda][T1][r,i]]/(4M[T1][r]^2),{r,1,n[T1]},{k,1,3}]+Sum[Conjugate[yHat[u][j,k]]\[Lambda][T2][r,k]Conjugate[\[Lambda][T2][r,i]]/(8M[T2][r]^2),{r,1,n[T2]},{k,1,3}]+I*Sum[Conjugate[yHat[u][j,k]]Im[gHat[B][\[Phi]][r]]g[B][q][r,i,k]/(M[B][r]^2),{r,1,n[B]},{k,1,3}]-I*Sum[Conjugate[yHat[u][k,i]]Im[gHat[B][\[Phi]][r]]g[B][u][r,k,j]/(M[B][r]^2),{r,1,n[B]},{k,1,3}]-I*Sum[Conjugate[yHat[u][j,k]]Im[gHat[W][\[Phi]][r]]g[W][q][r,i,k]/(4M[W][r]^2),{r,1,n[W]},{k,1,3}];
c[u][i_,j_]:=Sum[Conjugate[\[Kappa][S,\[CurlyPhi]][r,s]]\[Kappa][S][r]y[\[CurlyPhi]][u][s,i,j]/(M[S][r]^2M[\[CurlyPhi]][s]^2),{r,1,n[S]},{s,1,n[\[CurlyPhi]]}]+Sum[Conjugate[\[Kappa][\[CapitalXi],\[CurlyPhi]][s,r]]\[Kappa][\[CapitalXi]][s]y[\[CurlyPhi]][d][r,i,j]/(M[\[CurlyPhi]][r]^2M[\[CapitalXi]][s]^2),{s,1,n[\[CapitalXi]]},{r,1,n[\[CurlyPhi]]}]+2Sum[\[Kappa][\[CapitalXi]1,\[CurlyPhi]][r,s]Conjugate[\[Kappa][\[CapitalXi]1][r]]y[\[CurlyPhi]][u][s,i,j]/(M[\[CapitalXi]1][r]^2M[\[CurlyPhi]][s]^2),{r,1,n[\[CapitalXi]1]},{s,1,n[\[CurlyPhi]]}]-Sum[\[Lambda][U,Q1][r,s]Conjugate[\[Lambda][U][r,i]]\[Lambda][Q1][u][s,j]/(M[Q1][s]M[U][r]),{s,1,n[Q1]},{r,1,n[U]}]-Sum[\[Lambda][U,Q7][r,s]Conjugate[\[Lambda][U][r,i]]\[Lambda][Q7][s,j]/(M[Q7][s]M[U][r]),{r,1,n[U]},{s,1,n[Q7]}]-Sum[\[Lambda][T1,Q1][s,r]Conjugate[\[Lambda][T1][s,i]]\[Lambda][Q1][u][r,j]/(2M[T1][s]M[Q1][r]),{s,1,n[T1]},{r,1,n[Q1]}]-Sum[\[Lambda][T2,Q1][s,r]Conjugate[\[Lambda][T2][s,i]]\[Lambda][Q1][u][r,j]/(4M[Q1][r]M[T2][s]),{r,1,n[Q1]},{s,1,n[T2]}]+Sum[\[Lambda][T2,Q7][s,r]Conjugate[\[Lambda][T2][s,i]]\[Lambda][Q7][r,j]/(4M[T2][s]M[Q7][r]),{s,1,n[T2]},{r,1,n[Q7]}]-Sum[w[S,U][r,s,j]*\[Kappa][S][r]Conjugate[\[Lambda][U][s,i]]/(M[S][r]^2M[U][s]),{r,1,n[S]},{s,1,n[U]}]-Sum[Conjugate[w[S,Q1][r,s,i]]\[Kappa][S][r]Conjugate[\[Lambda][Q1][u][s,j]]/(M[S][r]^2M[Q1][s]),{r,1,n[S]},{s,1,n[Q1]}]-Sum[w[\[CapitalXi],T2][s,r,j]\[Kappa][\[CapitalXi]][s]Conjugate[\[Lambda][T2][r,i]]/(2M[\[CapitalXi]][s]^2M[T2][r]),{s,1,n[\[CapitalXi]]},{r,1,n[T2]}]+Sum[Conjugate[w[\[CapitalXi],Q7][r,s,i]]\[Kappa][\[CapitalXi]][r]\[Lambda][Q1][u][s,j]/(2M[\[CapitalXi]][r]^2M[Q1][s]),{r,1,n[\[CapitalXi]]},{s,1,n[Q1]}]-Sum[w[\[CapitalXi],T1][s,r,j]Conjugate[\[Kappa][\[CapitalXi]1][s]]Conjugate[\[Lambda][T1][r,i]]/(M[\[CapitalXi]1][s]^2M[T1][r]),{s,1,n[\[CapitalXi]1]},{r,1,n[T1]}]-2Sum[Conjugate[w[\[CapitalXi]1,Q7][r,s,i]]Conjugate[\[Kappa][\[CapitalXi]1][r]]\[Lambda][Q7][s,j]/(M[\[CapitalXi]1][r]^2M[Q7][s]),{r,1,n[\[CapitalXi]1]},{s,1,n[Q7]}]+I*Sum[Conjugate[yHat[u][j,k]]z[U,L1][r,s,k]Conjugate[\[Lambda][U][r,i]]\[Gamma][L1][s]/(2M[U][r]M[L1][s]^2),{r,1,n[U]},{s,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[u][j,k]]Conjugate[z[U,L1][r,s,i]]\[Lambda][U][r,k]Conjugate[\[Gamma][L1][s]]/(2M[U][r]M[L1][s]^2),{r,1,n[U]},{s,1,n[L1]},{k,1,3}]-I*Sum[Conjugate[yHat[u][k,i]]Conjugate[z[Q1,L1][u][s,r,k]]\[Lambda][Q1][u][s,j]\[Gamma][L1][r]/(2M[Q1][s]M[L1][r]^2),{s,1,n[Q1]},{r,1,n[L1]},{k,1,3}]-I*Sum[Conjugate[yHat[u][k,i]]z[Q1,L1][u][s,r,j]Conjugate[\[Lambda][Q1][u][s,k]]Conjugate[\[Gamma][L1][r]]/(2M[Q1][s]M[L1][r]^2),{s,1,n[Q1]},{r,1,n[L1]},{k,1,3}]-I*Sum[Conjugate[yHat[u][k,i]]Conjugate[z[Q7,L1][r,s,k]]\[Lambda][Q7][r,j]Conjugate[\[Gamma][L1][s]]/(2M[Q7][r]M[L1][s]^2),{r,1,n[Q7]},{s,1,n[L1]},{k,1,3}]-I*Sum[Conjugate[yHat[u][k,i]]z[Q7,L1][r,s,j]Conjugate[\[Lambda][Q7][r,k]]\[Gamma][L1][s]/(2M[Q7][r]M[L1][s]^2),{r,1,n[Q7]},{s,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[u][j,k]]z[T1,L1][r,s,k]Conjugate[\[Lambda][T1][r,i]]Conjugate[\[Gamma][L1][s]]/(2M[T1][r]M[L1][s]^2),{r,1,n[T1]},{s,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[u][j,k]]Conjugate[z[T1,L1][s,r,i]]\[Lambda][T1][s,k]\[Gamma][L1][r]/(2M[T1][s]M[L1][r]^2),{s,1,n[T1]},{r,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[u][j,k]]Conjugate[z[T2,L1][s,r,i]]\[Lambda][T2][s,k]Conjugate[\[Gamma][L1][r]]/(4M[T2][s]M[L1][r]^2),{s,1,n[T2]},{r,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[u][j,k]]z[T2,L1][r,s,k]Conjugate[\[Lambda][T2][r,i]]\[Gamma][L1][s]/(4M[T2][r]M[L1][s]^2),{r,1,n[T2]},{s,1,n[L1]},{k,1,3}]+I*Sum[Conjugate[yHat[u][j,k]]\[Delta][B,S][r,s]g[B][q][r,i,k]\[Kappa][S][s]/(M[B][r]^2M[S][s]^2),{r,1,n[B]},{s,1,n[S]},{k,1,3}]-I*Sum[Conjugate[yHat[u][k,i]]\[Delta][B,S][r,s]g[B][u][r,k,j]\[Kappa][S][s]/(M[B][r]^2M[S][s]^2),{r,1,n[B]},{s,1,n[S]},{k,1,3}]-I*Sum[Conjugate[yHat[u][j,k]]\[Delta][W,\[CapitalXi]][s,r]g[W][q][s,i,k]\[Kappa][\[CapitalXi]][r]/(2 M[\[CapitalXi]][r]^2M[W][s]^2),{r,1,n[\[CapitalXi]]},{s,1,n[W]},{k,1,3}]-2\[Lambda]Hat[\[Phi]]Sum[Conjugate[\[Delta][L1,\[CurlyPhi]][s,r]]Conjugate[\[Gamma][L1][s]]y[\[CurlyPhi]][u][r,j,i]/(M[\[CurlyPhi]][r]^2M[L1][s]^2),{r,1,n[\[CurlyPhi]]},{s,1,n[L1]}];
Subscript[wC, e\[Phi]][i_,j_]:=(Conjugate[yHat[e][i,j]]*a\[Psi]\[Phi]+b[e][i,j]+c[e][i,j])/(Z\[Phi]^(3/2));
Subscript[wC, d\[Phi]][i_,j_]:=(Conjugate[yHat[d][i,j]]*a\[Psi]\[Phi]+b[d][i,j]+c[d][i,j])/(Z\[Phi]^(3/2));
Subscript[wC, u\[Phi]][i_,j_]:=(Conjugate[yHat[u][i,j]]*a\[Psi]\[Phi]+b[u][i,j]+c[u][i,j])/(Z\[Phi]^(3/2));
Subscript[wC, eB][i_,j_]:=0;
Subscript[wC, eW][i_,j_]:=0;
Subscript[wC, dB][i_,j_]:=0;
Subscript[wC, uB][i_,j_]:=0;
Subscript[wC, uW][i_,j_]:=0;
Subscript[wC, dW][i_,j_]:=0;
Subscript[wC, dG][i_,j_]:=0;
Subscript[wC, uG][i_,j_]:=0;
Subscript[wC, \[Phi]L1][i_,j_]:=(Sum[Conjugate[\[Lambda][Ne][r,i]]\[Lambda][Ne][r,j]/(4M[Ne][r]^2),{r,1,n[Ne]}]-Sum[Conjugate[\[Lambda][Ef][r,i]]\[Lambda][Ef][r,j]/(4M[Ef][r]^2),{r,1,n[Ef]}]+3Sum[Conjugate[\[Lambda][\[CapitalSigma]][r,i]]\[Lambda][\[CapitalSigma]][r,j]/(16M[\[CapitalSigma]][r]^2),{r,1,n[\[CapitalSigma]]}]-3Sum[Conjugate[\[Lambda][\[CapitalSigma]1][r,i]]\[Lambda][\[CapitalSigma]1][r,j]/(16M[\[CapitalSigma]1][r]^2),{r,1,n[\[CapitalSigma]1]}]-Sum[Re[gHat[B][\[Phi]][r]]g[B][L][r,i,j]/(M[B][r]^2),{r,1,n[B]}]-Subscript[g1, SM] KroneckerDelta[i,j]Sum[g[L1][B][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(4M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]+I*Sum[\[Lambda][Ne][r,j]Conjugate[z[Ne,L1][r,s,i]]Conjugate[\[Gamma][L1][s]]/(4M[Ne][r]M[L1][s]^2),{r,1,n[Ne]},{s,1,n[L1]}]-I*Sum[Conjugate[\[Lambda][Ne][r,i]]z[Ne,L1][r,s,j]\[Gamma][L1][s]/(4M[Ne][r]M[L1][s]^2),{r,1,n[Ne]},{s,1,n[L1]}]-I*Sum[\[Lambda][Ef][r,j]Conjugate[z[Ef,L1][r,s,i]]\[Gamma][L1][s]/(4M[Ef][r]M[L1][s]^2),{r,1,n[Ef]},{s,1,n[L1]}]+I*Sum[Conjugate[\[Lambda][Ef][r,i]]z[Ef,L1][r,s,j]Conjugate[\[Gamma][L1][s]]/(4M[Ef][r]M[L1][s]^2),{r,1,n[Ef]},{s,1,n[L1]}]+3I*Sum[\[Lambda][\[CapitalSigma]][s,j]Conjugate[z[\[CapitalSigma],L1][s,r,i]]Conjugate[\[Gamma][L1][r]]/(8M[\[CapitalSigma]][s]M[L1][r]^2),{r,1,n[L1]},{s,1,n[\[CapitalSigma]]}]-3I*Sum[Conjugate[\[Lambda][\[CapitalSigma]][r,i]]z[\[CapitalSigma],L1][r,s,j]\[Gamma][L1][s]/(8M[\[CapitalSigma]][r]M[L1][s]^2),{s,1,n[L1]},{r,1,n[\[CapitalSigma]]}]+3I*Sum[Conjugate[\[Lambda][\[CapitalSigma]1][s,i]]z[\[CapitalSigma]1,L1][s,r,j]Conjugate[\[Gamma][L1][r]]/(8M[\[CapitalSigma]1][s]M[L1][r]^2),{r,1,n[L1]},{s,1,n[\[CapitalSigma]1]}]-3I*Sum[\[Lambda][\[CapitalSigma]1][r,j]Conjugate[z[\[CapitalSigma]1,L1][r,s,i]]\[Gamma][L1][s]/(8M[\[CapitalSigma]1][r]M[L1][s]^2),{s,1,n[L1]},{r,1,n[\[CapitalSigma]1]}])/Z\[Phi];
Subscript[wC, \[Phi]L3][i_,j_]:=(-Sum[Conjugate[\[Lambda][Ne][r,i]]\[Lambda][Ne][r,j]/(4M[Ne][r]^2),{r,1,n[Ne]}]-Sum[Conjugate[\[Lambda][Ef][r,i]]\[Lambda][Ef][r,j]/(4M[Ef][r]^2),{r,1,n[Ef]}]+Sum[Conjugate[\[Lambda][\[CapitalSigma]][r,i]]\[Lambda][\[CapitalSigma]][r,j]/(16M[\[CapitalSigma]][r]^2),{r,1,n[\[CapitalSigma]]}]+Sum[Conjugate[\[Lambda][\[CapitalSigma]1][r,i]]\[Lambda][\[CapitalSigma]1][r,j]/(16M[\[CapitalSigma]1][r]^2),{r,1,n[\[CapitalSigma]1]}]-Sum[Re[gHat[W][\[Phi]][r]]g[W][L][r,i,j]/(4M[W][r]^2),{r,1,n[W]}]+Subscript[g2, SM] KroneckerDelta[i,j]Sum[g[L1][W][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(4M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]-I*Sum[\[Lambda][Ne][r,j]Conjugate[z[Ne,L1][r,s,i]]Conjugate[\[Gamma][L1][s]]/(4M[Ne][r]M[L1][s]^2),{r,1,n[Ne]},{s,1,n[L1]}]+I*Sum[Conjugate[\[Lambda][Ne][r,i]]z[Ne,L1][r,s,j]\[Gamma][L1][s]/(4M[Ne][r]M[L1][s]^2),{r,1,n[Ne]},{s,1,n[L1]}]+I*Sum[Conjugate[\[Lambda][Ef][r,i]]z[Ef,L1][r,s,j]Conjugate[\[Gamma][L1][s]]/(4M[Ef][r]M[L1][s]^2),{r,1,n[Ef]},{s,1,n[L1]}]-I*Sum[\[Lambda][Ef][r,j]Conjugate[z[Ef,L1][r,s,i]]\[Gamma][L1][s]/(4M[Ef][r]M[L1][s]^2),{r,1,n[Ef]},{s,1,n[L1]}]+I*Sum[\[Lambda][\[CapitalSigma]][s,j]Conjugate[z[\[CapitalSigma],L1][s,r,i]]Conjugate[\[Gamma][L1][r]]/(8M[\[CapitalSigma]][s]M[L1][r]^2),{r,1,n[L1]},{s,1,n[\[CapitalSigma]]}]-I*Sum[Conjugate[\[Lambda][\[CapitalSigma]][r,i]]z[\[CapitalSigma],L1][r,s,j]\[Gamma][L1][s]/(8M[\[CapitalSigma]][r]M[L1][s]^2),{s,1,n[L1]},{r,1,n[\[CapitalSigma]]}]-I*Sum[Conjugate[\[Lambda][\[CapitalSigma]1][s,i]]z[\[CapitalSigma]1,L1][s,r,j]Conjugate[\[Gamma][L1][r]]/(8M[\[CapitalSigma]1][s]M[L1][r]^2),{r,1,n[L1]},{s,1,n[\[CapitalSigma]1]}]+I*Sum[\[Lambda][\[CapitalSigma]1][r,j]Conjugate[z[\[CapitalSigma]1,L1][r,s,i]]\[Gamma][L1][s]/(8M[\[CapitalSigma]1][r]M[L1][s]^2),{s,1,n[L1]},{r,1,n[\[CapitalSigma]1]}])/Z\[Phi];
Subscript[wC, \[Phi]q1][i_,j_]:=(Sum[\[Lambda][U][r,j]Conjugate[\[Lambda][U][r,i]]/(4M[U][r]^2),{r,1,n[U]}]-Sum[\[Lambda][Df][r,j]Conjugate[\[Lambda][Df][r,i]]/(4M[Df][r]^2),{r,1,n[Df]}]-3Sum[\[Lambda][T1][r,j]Conjugate[\[Lambda][T1][r,i]]/(16M[T1][r]^2),{r,1,n[T1]}]+3Sum[\[Lambda][T2][r,j]Conjugate[\[Lambda][T2][r,i]]/(16M[T2][r]^2),{r,1,n[T2]}]-Sum[Re[gHat[B][\[Phi]][r]]g[B][q][r,i,j]/(M[B][r]^2),{r,1,n[B]}]+Subscript[g1, SM] KroneckerDelta[i,j]Sum[g[L1][B][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(12M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]-I*Sum[Conjugate[\[Lambda][U][r,i]]z[U,L1][r,s,j]\[Gamma][L1][s]/(4M[U][r]M[L1][s]^2),{r,1,n[U]},{s,1,n[L1]}]+I*Sum[\[Lambda][U][r,j]Conjugate[z[U,L1][r,s,i]]Conjugate[\[Gamma][L1][s]]/(4M[U][r]M[L1][s]^2),{r,1,n[U]},{s,1,n[L1]}]+I*Sum[Conjugate[\[Lambda][Df][r,i]]z[Df,L1][r,s,j]Conjugate[\[Gamma][L1][s]]/(4M[Df][r]M[L1][s]^2),{r,1,n[Df]},{s,1,n[L1]}]-I*Sum[\[Lambda][Df][r,j]Conjugate[z[Df,L1][r,s,i]]\[Gamma][L1][s]/(4M[Df][r]M[L1][s]^2),{r,1,n[Df]},{s,1,n[L1]}]+3I*Sum[Conjugate[\[Lambda][T1][r,i]]z[T1,L1][r,s,j]Conjugate[\[Gamma][L1][s]]/(8M[T1][r]M[L1][s]^2),{r,1,n[T1]},{s,1,n[L1]}]-3I*Sum[\[Lambda][T1][s,j]Conjugate[z[T1,L1][s,r,i]]\[Gamma][L1][r]/(8M[T1][s]M[L1][r]^2),{r,1,n[L1]},{s,1,n[T1]}]-3I*Sum[Conjugate[\[Lambda][T2][r,i]]z[T2,L1][r,s,j]\[Gamma][L1][s]/(8M[T2][r]M[L1][s]^2),{s,1,n[L1]},{r,1,n[T2]}]+3I*Sum[\[Lambda][T2][s,j]Conjugate[z[T2,L1][s,r,i]]Conjugate[\[Gamma][L1][r]]/(8M[T2][s]M[L1][r]^2),{r,1,n[L1]},{s,1,n[T2]}])/Z\[Phi];
Subscript[wC, \[Phi]q3][i_,j_]:=(-Sum[\[Lambda][U][r,j]Conjugate[\[Lambda][U][r,i]]/(4M[U][r]^2),{r,1,n[U]}]-Sum[\[Lambda][Df][r,j]Conjugate[\[Lambda][Df][r,i]]/(4M[Df][r]^2),{r,1,n[Df]}]+Sum[\[Lambda][T1][r,j]Conjugate[\[Lambda][T1][r,i]]/(16M[T1][r]^2),{r,1,n[T1]}]+Sum[\[Lambda][T2][r,j]Conjugate[\[Lambda][T2][r,i]]/(16M[T2][r]^2),{r,1,n[T2]}]-Sum[Re[gHat[W][\[Phi]][r]]g[W][q][r,i,j]/(4M[W][r]^2),{r,1,n[W]}]+Subscript[g2, SM] KroneckerDelta[i,j]Sum[g[L1][W][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(4M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]+I*Sum[Conjugate[\[Lambda][U][r,i]]z[U,L1][r,s,j]\[Gamma][L1][s]/(4M[U][r]M[L1][s]^2),{r,1,n[U]},{s,1,n[L1]}]-I*Sum[\[Lambda][U][r,j]Conjugate[z[U,L1][r,s,i]]Conjugate[\[Gamma][L1][s]]/(4M[U][r]M[L1][s]^2),{r,1,n[U]},{s,1,n[L1]}]+I*Sum[Conjugate[\[Lambda][Df][r,i]]z[Df,L1][r,s,j]Conjugate[\[Gamma][L1][s]]/(4M[Df][r]M[L1][s]^2),{r,1,n[Df]},{s,1,n[L1]}]-I*Sum[\[Lambda][Df][r,j]Conjugate[z[Df,L1][r,s,i]]\[Gamma][L1][s]/(4M[Df][r]M[L1][s]^2),{r,1,n[Df]},{s,1,n[L1]}]-I*Sum[Conjugate[\[Lambda][T1][r,i]]z[T1,L1][r,s,j]Conjugate[\[Gamma][L1][s]]/(8M[T1][r]M[L1][s]^2),{r,1,n[T1]},{s,1,n[L1]}]+I*Sum[\[Lambda][T1][s,j]Conjugate[z[T1,L1][s,r,i]]\[Gamma][L1][r]/(8M[T1][s]M[L1][r]^2),{r,1,n[L1]},{s,1,n[T1]}]-I*Sum[Conjugate[\[Lambda][T2][r,i]]z[T2,L1][r,s,j]\[Gamma][L1][s]/(8M[T2][r]M[L1][s]^2),{s,1,n[L1]},{r,1,n[T2]}]+I*Sum[\[Lambda][T2][s,j]Conjugate[z[T2,L1][s,r,i]]Conjugate[\[Gamma][L1][r]]/(8M[T2][s]M[L1][r]^2),{r,1,n[L1]},{s,1,n[T2]}])/Z\[Phi];
Subscript[wC, \[Phi]e][i_,j_]:=(Sum[Conjugate[\[Lambda][\[CapitalDelta]1][r,i]]\[Lambda][\[CapitalDelta]1][r,j]/(2M[\[CapitalDelta]1][r]^2),{r,1,n[\[CapitalDelta]1]}]-Sum[Conjugate[\[Lambda][\[CapitalDelta]3][r,i]]\[Lambda][\[CapitalDelta]3][r,j]/(2M[\[CapitalDelta]3][r]^2),{r,1,n[\[CapitalDelta]3]}]-Sum[Re[gHat[B][\[Phi]][r]]g[B][e][r,i,j]/(M[B][r]^2),{r,1,n[B]}]-Subscript[g1, SM] KroneckerDelta[i,j]Sum[g[L1][B][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(2M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]+I*Sum[\[Lambda][\[CapitalDelta]1][r,j]Conjugate[z[\[CapitalDelta]1,L1][r,s,i]]Conjugate[\[Gamma][L1][s]]/(2M[\[CapitalDelta]1][r]M[L1][s]^2),{r,1,n[\[CapitalDelta]1]},{s,1,n[L1]}]-I*Sum[Conjugate[\[Lambda][\[CapitalDelta]1][r,i]]z[\[CapitalDelta]1,L1][r,s,j]\[Gamma][L1][s]/(2M[\[CapitalDelta]1][r]M[L1][s]^2),{r,1,n[\[CapitalDelta]1]},{s,1,n[L1]}]-I*Sum[\[Lambda][\[CapitalDelta]3][s,j]Conjugate[z[\[CapitalDelta]3,L1][s,r,i]]\[Gamma][L1][r]/(2M[\[CapitalDelta]3][s]M[L1][r]^2),{s,1,n[\[CapitalDelta]1]},{r,1,n[L1]}]+I*Sum[Conjugate[\[Lambda][\[CapitalDelta]3][s,i]]z[\[CapitalDelta]3,L1][s,r,j]Conjugate[\[Gamma][L1][r]]/(2M[\[CapitalDelta]3][s]M[L1][r]^2),{s,1,n[\[CapitalDelta]3]},{r,1,n[L1]}])/Z\[Phi];
Subscript[wC, \[Phi]d][i_,j_]:=(Sum[Conjugate[\[Lambda][Q1][d][r,i]]\[Lambda][Q1][d][r,j]/(2M[Q1][r]^2),{r,1,n[Q1]}]-Sum[Conjugate[\[Lambda][Q5][r,i]]\[Lambda][Q5][r,j]/(2M[Q5][r]^2),{r,1,n[Q5]}]-Sum[Re[gHat[B][\[Phi]][r]]g[B][d][r,i,j]/(M[B][r]^2),{r,1,n[B]}]-Subscript[g1, SM] KroneckerDelta[i,j]Sum[g[L1][B][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(6M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]+I*Sum[\[Lambda][Q1][d][r,j]Conjugate[z[Q1,L1][d][r,s,i]]Conjugate[\[Gamma][L1][s]]/(2M[Q1][r]M[L1][s]^2),{r,1,n[Q1]},{s,1,n[L1]}]
-I*Sum[Conjugate[\[Lambda][Q1][d][r,i]]z[Q1,L1][d][r,s,j]\[Gamma][L1][s]/(2M[Q1][r]M[L1][s]^2),{r,1,n[Q1]},{s,1,n[L1]}]-I*Sum[\[Lambda][Q5][s,j]Conjugate[z[Q5,L1][r,s,i]]\[Gamma][L1][s]/(2M[Q5][r]M[L1][s]^2),{r,1,n[Q5]},{s,1,n[L1]}]+I*Sum[Conjugate[\[Lambda][Q5][r,i]]z[Q5,L1][r,s,j]Conjugate[\[Gamma][L1][s]]/(2M[Q5][r]M[L1][s]^2),{r,1,n[Q5]},{s,1,n[L1]}])/Z\[Phi];
Subscript[wC, \[Phi]u][i_,j_]:=(-Sum[Conjugate[\[Lambda][Q1][u][r,i]]\[Lambda][Q1][u][r,j]/(2M[Q1][r]^2),{r,1,n[Q1]}]+Sum[Conjugate[\[Lambda][Q7][r,i]]\[Lambda][Q7][r,j]/(2M[Q7][r]^2),{r,1,n[Q7]}]-Sum[Re[gHat[B][\[Phi]][r]]g[B][u][r,i,j]/(M[B][r]^2),{r,1,n[B]}]+Subscript[g1, SM] KroneckerDelta[i,j]Sum[g[L1][B][r,s]Conjugate[\[Gamma][L1][r]]\[Gamma][L1][s]/(3M[L1][r]^2M[L1][s]^2),{r,1,n[L1]},{s,1,n[L1]}]-I*Sum[\[Lambda][Q1][u][s,j]Conjugate[z[Q1,L1][u][s,r,i]]Conjugate[\[Gamma][L1][r]]/(2M[Q1][s]M[L1][r]^2),{s,1,n[Q1]},{r,1,n[L1]}]+I*Sum[Conjugate[\[Lambda][Q1][u][s,i]]z[Q1,L1][u][s,r,j]Conjugate[\[Gamma][L1][r]]/(2M[Q1][s]M[L1][r]^2),{s,1,n[Q1]},{r,1,n[L1]}]+I*Sum[\[Lambda][Q7][r,j]Conjugate[z[Q7,L1][r,s,i]]Conjugate[\[Gamma][L1][s]]/(2M[Q7][r]M[L1][s]^2),{r,1,n[Q7]},{s,1,n[L1]}]-I*Sum[Conjugate[\[Lambda][Q7][r,i]]z[Q7,L1][r,s,j]\[Gamma][L1][s]/(2M[Q7][r]M[L1][s]^2),{r,1,n[Q7]},{s,1,n[L1]}])/Z\[Phi];
Subscript[wC, \[Phi]ud][i_,j_]:=(Sum[Conjugate[\[Lambda][Q1][u][r,i]]\[Lambda][Q1][d][r,j]/(M[Q1][r]^2),{r,1,n[Q1]}]-Sum[gHat[B1][\[Phi]][r]g[B1][du][r,j,i]/(M[B1][r]^2),{r,1,n[B1]}]+I*Sum[\[Lambda][Q1][d][r,j]Conjugate[z[Q1,L1][u][r,s,i]]\[Gamma][L1][s]/(M[Q1][r]M[L1][s]^2),{r,1,n[Q1]},{s,1,n[L1]}]-I*Sum[Conjugate[\[Lambda][Q1][u][r,i]]z[Q1,L1][d][r,s,j]\[Gamma][L1][s]/(M[Q1][r]M[L1][s]^2),{r,1,n[Q1]},{s,1,n[L1]}])/Z\[Phi];
n[S]=0;
listM[S]={0}; (*In TeV*)
n[S1]=0;
listM[S1]={0};
n[S2]=0;
listM[S2]={0};
n[\[CurlyPhi]]=0;
listM[\[CurlyPhi]]={0};
n[\[CapitalXi]]=0;
listM[\[CapitalXi]]={0};
n[\[CapitalXi]1]=0;
listM[\[CapitalXi]1]={0};
n[\[CapitalTheta]1]=0;
listM[\[CapitalTheta]1]={0};
n[\[CapitalTheta]3]=0;
listM[\[CapitalTheta]3]={0};
n[\[Omega]1]=0;
listM[\[Omega]1]={0};
n[\[Omega]2]=0;
listM[\[Omega]2]={0};
n[\[Omega]4]=0;
listM[\[Omega]4]={0};
n[\[CapitalPi]1]=0;
listM[\[CapitalPi]1]={0};
n[\[CapitalPi]7]=0;
listM[\[CapitalPi]7]={0};
n[\[Zeta]]=0;
listM[\[Zeta]]={0};
n[\[CapitalOmega]1]=0;
listM[\[CapitalOmega]1]={0};
n[\[CapitalOmega]2]=0;
listM[\[CapitalOmega]2]={0};
n[\[CapitalOmega]4]=0;
listM[\[CapitalOmega]4]={0};
n[\[CapitalUpsilon]]=0;
listM[\[CapitalUpsilon]]={0};
n[\[CapitalPhi]]=0;
listM[\[CapitalPhi]]={0};
n[B]=0;
listM[B]={0};
n[B1]=0;
listM[B1]={0};
n[W]=0;
listM[W]={0};
n[W1]=0;
listM[W1]={0};
n[G]=0;
listM[G]={0};
n[G1]=0;
listM[G1]={0};
n[H]=0;
listM[H]={0};
n[L1]=0;
listM[L1]={0};
n[L3]=0;
listM[L3]={0};
n[\[ScriptCapitalU]2]=0;
listM[\[ScriptCapitalU]2]={0};
n[\[ScriptCapitalU]5]=0;
listM[\[ScriptCapitalU]5]={0};
n[\[ScriptCapitalQ]1]=0;
listM[\[ScriptCapitalQ]1]={0};
n[\[ScriptCapitalQ]5]=0;
listM[\[ScriptCapitalQ]5]={0};
n[X]=0;
listM[X]={0};
n[Y1]=0;
listM[Y1]={0};
n[Y5]=0;
listM[Y5]={0};
n[Ne]=0;
listM[Ne]={0};
n[Ef]=0;
listM[Ef]={0};
n[\[CapitalDelta]1]=0;
listM[\[CapitalDelta]1]={0};
n[\[CapitalDelta]3]=0;
listM[\[CapitalDelta]3]={0};
n[\[CapitalSigma]]=0;
listM[\[CapitalSigma]]={0};
n[\[CapitalSigma]1]=0;
listM[\[CapitalSigma]1]={0};
n[U]=0;
listM[U]={0};
n[Df]=0;
listM[Df]={0};
n[Q1]=0;
listM[Q1]={0};
n[Q5]=0;
listM[Q5]={0};
n[Q7]=0;
listM[Q7]={0};
n[T1]=0;
listM[T1]={0};
n[T2]=0;
listM[T2]={0};
(*Made to match explicitly the assumptions and operators in 2105.00006.
It allows corrections to bottom, charm and tau Yukawas.*)
toVanishLeptonsLL[Fl2]=Block[{assoc,allowed},assoc=Flatten[Table[{i,j,k,l},{i,1,3},{j,1,3},{k,1,3},{l,1,3}],3];
allowed=Select[assoc,((#[[1]]==#[[2]] && #[[3]]==#[[4]])||(#[[1]]==#[[4]]&&#[[2]]==#[[3]]))&];
Select[assoc,FreeQ[allowed,#]&]];
toVanishLeptonsLLRR[Fl2]=Block[{assoc,allowed},assoc=Flatten[Table[{i,j,k,l},{i,1,3},{j,1,3},{k,1,3},{l,1,3}],3];
allowed=Select[assoc,((#[[1]]==#[[2]] && #[[3]]==#[[4]]))&];
Select[assoc,FreeQ[allowed,#]&]];
toVanishQuarksLL[Fl2]=Block[{assoc,allowed},assoc=Flatten[Table[{i,j,k,l},{i,1,3},{j,1,3},{k,1,3},{l,1,3}],3];
allowed=Select[assoc,((#[[1]]==#[[2]] && #[[3]]==#[[4]])||(#[[1]]==#[[4]]&&#[[2]]==#[[3]]))&];
Select[assoc,FreeQ[allowed,#]&]];
toVanishQuarkLeptonsLL[Fl2]=Block[{assoc,allowed},assoc=Flatten[Table[{i,j,k,l},{i,1,3},{j,1,3},{k,1,3},{l,1,3}],3];
allowed=Select[assoc,((#[[1]]==#[[2]] && #[[3]]==#[[4]]))&];
Select[assoc,FreeQ[allowed,#]&]];
toVanishQuarkDownsRR[Fl2]=Block[{assoc,allowed},assoc=Flatten[Table[{i,j,k,l},{i,1,3},{j,1,3},{k,1,3},{l,1,3}],3];
allowed=Select[assoc,((#[[1]]==#[[2]] && #[[3]]==#[[4]])||(#[[1]]==#[[4]]&&#[[2]]==#[[3]]))&];
Select[assoc,FreeQ[allowed,#]&]];
toVanishQuarkUpsRR[Fl2]=Block[{assoc,allowed},assoc=Flatten[Table[{i,j,k,l},{i,1,3},{j,1,3},{k,1,3},{l,1,3}],3];
allowed=Select[assoc,((#[[1]]==#[[2]] && #[[3]]==#[[4]])||(#[[1]]==#[[4]]&&#[[2]]==#[[3]]))&];
Select[assoc,FreeQ[allowed,#]&]];
toVanishQuarkUpDownRR[Fl2]=Block[{assoc,allowed},assoc=Flatten[Table[{i,j,k,l},{i,1,3},{j,1,3},{k,1,3},{l,1,3}],3];
allowed=Select[assoc,((#[[1]]==#[[2]] && #[[3]]==#[[4]]))&];
Select[assoc,FreeQ[allowed,#]&]];
toVanishQuarkLeptonRR[Fl2]=Block[{assoc,allowed},assoc=Flatten[Table[{i,j,k,l},{i,1,3},{j,1,3},{k,1,3},{l,1,3}],3];
allowed=Select[assoc,((#[[1]]==#[[2]] && #[[3]]==#[[4]]))&];
Select[assoc,FreeQ[allowed,#]&]];
toVanishQuarkLeptonLLRR[Fl2]=Block[{assoc,allowed},assoc=Flatten[Table[{i,j,k,l},{i,1,3},{j,1,3},{k,1,3},{l,1,3}],3];
allowed=Select[assoc,((#[[1]]==#[[2]] && #[[3]]==#[[4]]))&];
Select[assoc,FreeQ[allowed,#]&]];
toVanishQuarksLLRR[Fl2]=Block[{assoc,allowed},assoc=Flatten[Table[{i,j,k,l},{i,1,3},{j,1,3},{k,1,3},{l,1,3}],3];
allowed=Select[assoc,((#[[1]]==#[[2]] && #[[3]]==#[[4]]))&];
Select[assoc,FreeQ[allowed,#]&]];
toVanishLRRL[Fl2]=Block[{assoc,allowed},assoc=Flatten[Table[{i,j,k,l},{i,1,3},{j,1,3},{k,1,3},{l,1,3}],3];
allowed=Select[assoc,False];
(*Print[Length[allowed]];*)
Select[assoc,FreeQ[allowed,#]&]];
toVanishLRLR[Fl2]=Block[{assoc,allowed},assoc=Flatten[Table[{i,j,k,l},{i,1,3},{j,1,3},{k,1,3},{l,1,3}],3];
allowed=Select[assoc,False];
(*Print[Length[allowed]];*)
Select[assoc,FreeQ[allowed,#]&]];
assumOpe[Fl2,Subscript[wwC, \[Phi]q3]]:={Subscript[wC, \[Phi]q3][1,1]==Subscript[wC, \[Phi]q3][2,2],
									Subscript[wC, \[Phi]q3][1,2]==0,Subscript[wC, \[Phi]q3][2,1]==0,
									Subscript[wC, \[Phi]q3][1,3]==0,Subscript[wC, \[Phi]q3][3,1]==0,
									Subscript[wC, \[Phi]q3][2,3]==0,Subscript[wC, \[Phi]q3][3,2]==0}/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, \[Phi]q1]]:={Subscript[wC, \[Phi]q1][1,1]==Subscript[wC, \[Phi]q1][2,2],
									Subscript[wC, \[Phi]q1][1,2]==0,Subscript[wC, \[Phi]q1][2,1]==0,
									Subscript[wC, \[Phi]q1][1,3]==0,Subscript[wC, \[Phi]q1][3,1]==0,
									Subscript[wC, \[Phi]q1][2,3]==0,Subscript[wC, \[Phi]q1][3,2]==0}/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, \[Phi]u]]:={Subscript[wC, \[Phi]u][1,1]==Subscript[wC, \[Phi]u][2,2],
								   Subscript[wC, \[Phi]u][1,2]==0,Subscript[wC, \[Phi]u][2,1]==0,
								   Subscript[wC, \[Phi]u][1,3]==0,Subscript[wC, \[Phi]u][3,1]==0,
								   Subscript[wC, \[Phi]u][2,3]==0,Subscript[wC, \[Phi]u][3,2]==0}/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, \[Phi]d]]:={Subscript[wC, \[Phi]d][1,1]==Subscript[wC, \[Phi]d][2,2]==Subscript[wC, \[Phi]d][3,3],
									Subscript[wC, \[Phi]d][1,2]==0,Subscript[wC, \[Phi]d][2,1]==0,Subscript[wC, \[Phi]d][1,3]==0,
									Subscript[wC, \[Phi]d][3,1]==0,Subscript[wC, \[Phi]d][2,3]==0,Subscript[wC, \[Phi]d][3,2]==0}/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, \[Phi]e]]:={Subscript[wC, \[Phi]e][1,2]==0,Subscript[wC, \[Phi]e][2,1]==0,Subscript[wC, \[Phi]e][1,3]==0,Subscript[wC, \[Phi]e][3,1]==0,Subscript[wC, \[Phi]e][2,3]==0,Subscript[wC, \[Phi]e][3,2]==0}/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, \[Phi]L1]]:={Subscript[wC, \[Phi]L1][1,2]==0,Subscript[wC, \[Phi]L1][2,1]==0,Subscript[wC, \[Phi]L1][1,3]==0,Subscript[wC, \[Phi]L1][3,1]==0,Subscript[wC, \[Phi]L1][2,3]==0,Subscript[wC, \[Phi]L1][3,2]==0}/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, \[Phi]L3]]:={Subscript[wC, \[Phi]L3][1,2]==0,Subscript[wC, \[Phi]L3][2,1]==0,Subscript[wC, \[Phi]L3][1,3]==0,Subscript[wC, \[Phi]L3][3,1]==0,Subscript[wC, \[Phi]L3][2,3]==0,Subscript[wC, \[Phi]L3][3,2]==0}/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, e\[Phi]]]:={Subscript[wC, e\[Phi]][1,1]==0,Subscript[wC, e\[Phi]][1,2]==0,Subscript[wC, e\[Phi]][1,3]==0,Subscript[wC, e\[Phi]][2,1]==0,Subscript[wC, e\[Phi]][2,2]==0,Subscript[wC, e\[Phi]][2,3]==0,Subscript[wC, e\[Phi]][3,1]==0,Subscript[wC, e\[Phi]][3,2]==0}/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, d\[Phi]]]:={Subscript[wC, d\[Phi]][1,1]==0,Subscript[wC, d\[Phi]][1,2]==0,Subscript[wC, d\[Phi]][1,3]==0,Subscript[wC, d\[Phi]][2,1]==0,Subscript[wC, d\[Phi]][2,2]==0,Subscript[wC, d\[Phi]][2,3]==0,Subscript[wC, d\[Phi]][3,1]==0,Subscript[wC, d\[Phi]][3,2]==0}/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, u\[Phi]]]:={Subscript[wC, u\[Phi]][1,1]==0,Subscript[wC, u\[Phi]][1,2]==0,Subscript[wC, u\[Phi]][1,3]==0,Subscript[wC, u\[Phi]][2,1]==0,Subscript[wC, u\[Phi]][2,3]==0,Subscript[wC, u\[Phi]][3,1]==0,Subscript[wC, u\[Phi]][3,2]==0}/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, \[Phi]ud]]:={Subscript[wC, \[Phi]ud][1,1]==0,Subscript[wC, \[Phi]ud][1,2]==0,Subscript[wC, \[Phi]ud][1,3]==0,Subscript[wC, \[Phi]ud][2,1]==0,Subscript[wC, \[Phi]ud][2,2]==0,Subscript[wC, \[Phi]ud][2,3]==0,Subscript[wC, \[Phi]ud][3,1]==0,Subscript[wC, \[Phi]ud][3,2]==0,Subscript[wC, \[Phi]ud][3,3]==0}/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, ll]]:=Table[Subscript[wC, ll][toVanishLeptonsLL[Fl2][[i,1]],toVanishLeptonsLL[Fl2][[i,2]],toVanishLeptonsLL[Fl2][[i,3]],toVanishLeptonsLL[Fl2][[i,4]]]==0,{i,1,Length[toVanishLeptonsLL[Fl2]]}]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, ee]]:=Table[Subscript[wC, ee][toVanishLeptonsLL[Fl2][[i,1]],toVanishLeptonsLL[Fl2][[i,2]],toVanishLeptonsLL[Fl2][[i,3]],toVanishLeptonsLL[Fl2][[i,4]]]==0,{i,1,Length[toVanishLeptonsLL[Fl2]]}]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, le]]:=Table[Subscript[wC, le][toVanishLeptonsLLRR[Fl2][[i,1]],toVanishLeptonsLLRR[Fl2][[i,2]],toVanishLeptonsLLRR[Fl2][[i,3]],toVanishLeptonsLLRR[Fl2][[i,4]]]==0,{i,1,Length[toVanishLeptonsLLRR[Fl2]]}]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, qq1]]:=Join[{Subscript[wC, qq1][1,1,1,1]==Subscript[wC, qq1][2,2,2,2],2*Subscript[wC, qq1][1,1,1,1]==Subscript[wC, qq1][1,1,2,2]+Subscript[wC, qq1][1,2,2,1],Subscript[wC, qq1][1,1,3,3]==Subscript[wC, qq1][2,2,3,3],Subscript[wC, qq1][1,3,3,1]==Subscript[wC, qq1][2,3,3,2]},Table[Subscript[wC, qq1][toVanishQuarksLL[Fl2][[i,1]],toVanishQuarksLL[Fl2][[i,2]],toVanishQuarksLL[Fl2][[i,3]],toVanishQuarksLL[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarksLL[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, qq3]]:=Join[{Subscript[wC, qq3][1,1,1,1]==Subscript[wC, qq3][2,2,2,2],2*Subscript[wC, qq3][1,1,1,1]==Subscript[wC, qq3][1,1,2,2]+Subscript[wC, qq3][1,2,2,1],Subscript[wC, qq3][1,1,3,3]==Subscript[wC, qq3][2,2,3,3],Subscript[wC, qq3][1,3,3,1]==Subscript[wC, qq3][2,3,3,2]},Table[Subscript[wC, qq3][toVanishQuarksLL[Fl2][[i,1]],toVanishQuarksLL[Fl2][[i,2]],toVanishQuarksLL[Fl2][[i,3]],toVanishQuarksLL[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarksLL[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, lq1]]:=Join[Table[Subscript[wC, lq1][i,i,1,1]==Subscript[wC, lq1][i,i,2,2],{i,1,3}],Table[Subscript[wC, lq1][toVanishQuarkLeptonsLL[Fl2][[i,1]],toVanishQuarkLeptonsLL[Fl2][[i,2]],toVanishQuarkLeptonsLL[Fl2][[i,3]],toVanishQuarkLeptonsLL[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarkLeptonsLL[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, lq3]]:=Join[Table[Subscript[wC, lq3][i,i,1,1]==Subscript[wC, lq3][i,i,2,2],{i,1,3}],Table[Subscript[wC, lq3][toVanishQuarkLeptonsLL[Fl2][[i,1]],toVanishQuarkLeptonsLL[Fl2][[i,2]],toVanishQuarkLeptonsLL[Fl2][[i,3]],toVanishQuarkLeptonsLL[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarkLeptonsLL[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, dd]]:=Join[{Subscript[wC, dd][1,1,1,1]==Subscript[wC, dd][2,2,2,2],Subscript[wC, dd][3,3,3,3]==Subscript[wC, dd][2,2,2,2],2*Subscript[wC, dd][1,1,1,1]==Subscript[wC, dd][1,1,2,2]+Subscript[wC, dd][1,2,2,1],Subscript[wC, dd][1,1,2,2]==Subscript[wC, dd][1,1,3,3],Subscript[wC, dd][1,1,3,3]==Subscript[wC, dd][2,2,3,3],Subscript[wC, dd][1,2,2,1]==Subscript[wC, dd][1,3,3,1],Subscript[wC, dd][1,2,2,1]==Subscript[wC, dd][2,3,3,2]},Table[Subscript[wC, dd][toVanishQuarkDownsRR[Fl2][[i,1]],toVanishQuarkDownsRR[Fl2][[i,2]],toVanishQuarkDownsRR[Fl2][[i,3]],toVanishQuarkDownsRR[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarkDownsRR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, uu]]:=Join[{Subscript[wC, uu][1,1,1,1]==Subscript[wC, uu][2,2,2,2],2*Subscript[wC, uu][1,1,1,1]==Subscript[wC, uu][1,1,2,2]+Subscript[wC, uu][1,2,2,1],Subscript[wC, uu][1,1,3,3]==Subscript[wC, uu][2,2,3,3],Subscript[wC, uu][1,3,3,1]==Subscript[wC, uu][2,3,3,2]},Table[Subscript[wC, uu][toVanishQuarkUpsRR[Fl2][[i,1]],toVanishQuarkUpsRR[Fl2][[i,2]],toVanishQuarkUpsRR[Fl2][[i,3]],toVanishQuarkUpsRR[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarkUpsRR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, ud1]]:=Join[{Subscript[wC, ud1][1,1,1,1]==Subscript[wC, ud1][2,2,2,2],Subscript[wC, ud1][1,1,1,1]==Subscript[wC, ud1][1,1,2,2],Subscript[wC, ud1][1,1,3,3]==Subscript[wC, ud1][1,1,1,1],Subscript[wC, ud1][2,2,1,1]==Subscript[wC, ud1][1,1,1,1],Subscript[wC, ud1][2,2,3,3]==Subscript[wC, ud1][1,1,1,1],Subscript[wC, ud1][2,2,2,2]==Subscript[wC, ud1][1,1,1,1],Subscript[wC, ud1][3,3,1,1]==Subscript[wC, ud1][3,3,2,2],Subscript[wC, ud1][3,3,1,1]==Subscript[wC, ud1][3,3,3,3]},Table[Subscript[wC, ud1][toVanishQuarkUpDownRR[Fl2][[i,1]],toVanishQuarkUpDownRR[Fl2][[i,2]],toVanishQuarkUpDownRR[Fl2][[i,3]],toVanishQuarkUpDownRR[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarkUpDownRR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, ud8]]:=Join[{Subscript[wC, ud8][1,1,1,1]==Subscript[wC, ud8][2,2,2,2],Subscript[wC, ud8][1,1,1,1]==Subscript[wC, ud8][1,1,2,2],Subscript[wC, ud8][1,1,3,3]==Subscript[wC, ud8][1,1,1,1],Subscript[wC, ud8][2,2,1,1]==Subscript[wC, ud8][1,1,1,1],Subscript[wC, ud8][2,2,3,3]==Subscript[wC, ud8][1,1,1,1],Subscript[wC, ud8][2,2,2,2]==Subscript[wC, ud8][1,1,1,1],Subscript[wC, ud8][3,3,1,1]==Subscript[wC, ud8][3,3,2,2],Subscript[wC, ud8][3,3,1,1]==Subscript[wC, ud8][3,3,3,3]},Table[Subscript[wC, ud8][toVanishQuarkUpDownRR[Fl2][[i,1]],toVanishQuarkUpDownRR[Fl2][[i,2]],toVanishQuarkUpDownRR[Fl2][[i,3]],toVanishQuarkUpDownRR[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarkUpDownRR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, eu]]:=Join[{Subscript[wC, eu][1,1,1,1]==Subscript[wC, eu][1,1,2,2],Subscript[wC, eu][2,2,1,1]==Subscript[wC, eu][2,2,2,2],Subscript[wC, eu][3,3,1,1]==Subscript[wC, eu][3,3,2,2]},Table[Subscript[wC, eu][toVanishQuarkLeptonRR[Fl2][[i,1]],toVanishQuarkLeptonRR[Fl2][[i,2]],toVanishQuarkLeptonRR[Fl2][[i,3]],toVanishQuarkLeptonRR[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarkLeptonRR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, ed]]:=Join[{Subscript[wC, ed][1,1,1,1]==Subscript[wC, ed][1,1,2,2],Subscript[wC, ed][1,1,1,1]==Subscript[wC, ed][1,1,3,3],Subscript[wC, ed][2,2,1,1]==Subscript[wC, ed][2,2,2,2],Subscript[wC, ed][2,2,1,1]==Subscript[wC, ed][2,2,3,3],Subscript[wC, ed][3,3,3,3]==Subscript[wC, ed][3,3,2,2],Subscript[wC, ed][3,3,1,1]==Subscript[wC, ed][3,3,2,2]},Table[Subscript[wC, ed][toVanishQuarkLeptonRR[Fl2][[i,1]],toVanishQuarkLeptonRR[Fl2][[i,2]],toVanishQuarkLeptonRR[Fl2][[i,3]],toVanishQuarkLeptonRR[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarkLeptonRR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, lu]]:=Join[{Subscript[wC, lu][1,1,1,1]==Subscript[wC, lu][1,1,2,2],Subscript[wC, lu][2,2,1,1]==Subscript[wC, lu][2,2,2,2],Subscript[wC, lu][3,3,1,1]==Subscript[wC, lu][3,3,2,2]},Table[Subscript[wC, lu][toVanishQuarkLeptonLLRR[Fl2][[i,1]],toVanishQuarkLeptonLLRR[Fl2][[i,2]],toVanishQuarkLeptonLLRR[Fl2][[i,3]],toVanishQuarkLeptonLLRR[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarkLeptonLLRR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, ld]]:=Join[{Subscript[wC, ld][1,1,1,1]==Subscript[wC, ld][1,1,2,2],Subscript[wC, ld][1,1,1,1]==Subscript[wC, ld][1,1,3,3],Subscript[wC, ld][2,2,1,1]==Subscript[wC, ld][2,2,2,2],Subscript[wC, ld][2,2,1,1]==Subscript[wC, ld][2,2,3,3],Subscript[wC, ld][3,3,1,1]==Subscript[wC, ld][3,3,2,2],Subscript[wC, ld][3,3,1,1]==Subscript[wC, ld][3,3,3,3]},Table[Subscript[wC, ld][toVanishQuarkLeptonLLRR[Fl2][[i,1]],toVanishQuarkLeptonLLRR[Fl2][[i,2]],toVanishQuarkLeptonLLRR[Fl2][[i,3]],toVanishQuarkLeptonLLRR[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarkLeptonLLRR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, qe]]:=Join[{Subscript[wC, qe][1,1,1,1]==Subscript[wC, qe][2,2,1,1],Subscript[wC, qe][1,1,2,2]==Subscript[wC, qe][2,2,2,2],Subscript[wC, qe][1,1,3,3]==Subscript[wC, qe][2,2,3,3]},Table[Subscript[wC, qe][toVanishQuarkLeptonLLRR[Fl2][[i,1]],toVanishQuarkLeptonLLRR[Fl2][[i,2]],toVanishQuarkLeptonLLRR[Fl2][[i,3]],toVanishQuarkLeptonLLRR[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarkLeptonLLRR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, qu1]]:=Join[{Subscript[wC, qu1][1,1,1,1]==Subscript[wC, qu1][2,2,1,1],Subscript[wC, qu1][1,1,2,2]==Subscript[wC, qu1][2,2,2,2],Subscript[wC, qu1][1,1,2,2]==Subscript[wC, qu1][1,1,1,1],Subscript[wC, qu1][1,1,3,3]==Subscript[wC, qu1][2,2,3,3],Subscript[wC, qu1][3,3,1,1]==Subscript[wC, qu1][3,3,2,2]},Table[Subscript[wC, qu1][toVanishQuarksLLRR[Fl2][[i,1]],toVanishQuarksLLRR[Fl2][[i,2]],toVanishQuarksLLRR[Fl2][[i,3]],toVanishQuarksLLRR[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarksLLRR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, qu8]]:=Join[{Subscript[wC, qu8][1,1,1,1]==Subscript[wC, qu8][2,2,1,1],Subscript[wC, qu8][1,1,2,2]==Subscript[wC, qu8][2,2,2,2],Subscript[wC, qu8][1,1,2,2]==Subscript[wC, qu8][1,1,1,1],Subscript[wC, qu8][1,1,3,3]==Subscript[wC, qu8][2,2,3,3],Subscript[wC, qu8][3,3,1,1]==Subscript[wC, qu8][3,3,2,2]},Table[Subscript[wC, qu8][toVanishQuarksLLRR[Fl2][[i,1]],toVanishQuarksLLRR[Fl2][[i,2]],toVanishQuarksLLRR[Fl2][[i,3]],toVanishQuarksLLRR[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarksLLRR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, qd1]]:=Join[{Subscript[wC, qd1][1,1,1,1]==Subscript[wC, qd1][2,2,1,1],Subscript[wC, qd1][1,1,2,2]==Subscript[wC, qd1][2,2,2,2],Subscript[wC, qd1][1,1,2,2]==Subscript[wC, qd1][1,1,1,1],Subscript[wC, qd1][1,1,3,3]==Subscript[wC, qd1][2,2,3,3],Subscript[wC, qd1][1,1,3,3]==Subscript[wC, qd1][2,2,1,1],Subscript[wC, qd1][3,3,1,1]==Subscript[wC, qd1][3,3,2,2],Subscript[wC, qd1][3,3,1,1]==Subscript[wC, qd1][3,3,3,3]},Table[Subscript[wC, qd1][toVanishQuarksLLRR[Fl2][[i,1]],toVanishQuarksLLRR[Fl2][[i,2]],toVanishQuarksLLRR[Fl2][[i,3]],toVanishQuarksLLRR[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarksLLRR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, qd8]]:=Join[{Subscript[wC, qd8][1,1,1,1]==Subscript[wC, qd8][2,2,1,1],Subscript[wC, qd8][1,1,2,2]==Subscript[wC, qd8][2,2,2,2],Subscript[wC, qd8][1,1,2,2]==Subscript[wC, qd8][1,1,1,1],Subscript[wC, qd8][1,1,3,3]==Subscript[wC, qd8][2,2,3,3],Subscript[wC, qd8][1,1,3,3]==Subscript[wC, qd8][2,2,1,1],Subscript[wC, qd8][3,3,1,1]==Subscript[wC, qd8][3,3,2,2],Subscript[wC, qd8][3,3,1,1]==Subscript[wC, qd8][3,3,3,3]},Table[Subscript[wC, qd8][toVanishQuarksLLRR[Fl2][[i,1]],toVanishQuarksLLRR[Fl2][[i,2]],toVanishQuarksLLRR[Fl2][[i,3]],toVanishQuarksLLRR[Fl2][[i,4]]]==0,{i,1,Length[toVanishQuarksLLRR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, ledq]]:=Join[Table[Subscript[wC, ledq][toVanishLRRL[Fl2][[i,1]],toVanishLRRL[Fl2][[i,2]],toVanishLRRL[Fl2][[i,3]],toVanishLRRL[Fl2][[i,4]]]==0,{i,1,Length[toVanishLRRL[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, quqd1]]:=Join[Table[Subscript[wC, quqd1][toVanishLRLR[Fl2][[i,1]],toVanishLRLR[Fl2][[i,2]],toVanishLRLR[Fl2][[i,3]],toVanishLRLR[Fl2][[i,4]]]==0,{i,1,Length[toVanishLRLR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, quqd8]]:=Join[Table[Subscript[wC, quqd8][toVanishLRLR[Fl2][[i,1]],toVanishLRLR[Fl2][[i,2]],toVanishLRLR[Fl2][[i,3]],toVanishLRLR[Fl2][[i,4]]]==0,{i,1,Length[toVanishLRLR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, lequ1]]:=Join[Table[Subscript[wC, lequ1][toVanishLRLR[Fl2][[i,1]],toVanishLRLR[Fl2][[i,2]],toVanishLRLR[Fl2][[i,3]],toVanishLRLR[Fl2][[i,4]]]==0,{i,1,Length[toVanishLRLR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
assumOpe[Fl2,Subscript[wwC, lequ3]]:=Join[Table[Subscript[wC, lequ3][toVanishLRLR[Fl2][[i,1]],toVanishLRLR[Fl2][[i,2]],toVanishLRLR[Fl2][[i,3]],toVanishLRLR[Fl2][[i,4]]]==0,{i,1,Length[toVanishLRLR[Fl2]]}]]/.{ySM[a_][i_,j_]:>y[a][i,j]};
superSolAsummOpe[Fl_]:=Block[{allSol,preCond1,preCond2,preCond3,preCond4,preCond5,preCond6,preCond6a,preCond6b,preCond6c,preCond6d,preCond7,indPreSol1,auxTab1,auxTab2,numCond},
preCond1=DeleteCases[DeleteDuplicates[Join[assumOpe[Fl,Subscript[wwC, \[Phi]q3]],assumOpe[Fl,Subscript[wwC, \[Phi]q1]],assumOpe[Fl,Subscript[wwC, \[Phi]u]],assumOpe[Fl,Subscript[wwC, \[Phi]d]],assumOpe[Fl,Subscript[wwC, \[Phi]e]],assumOpe[Fl,Subscript[wwC, \[Phi]L1]],assumOpe[Fl,Subscript[wwC, \[Phi]L3]],assumOpe[Fl,Subscript[wwC, e\[Phi]]],assumOpe[Fl,Subscript[wwC, d\[Phi]]],assumOpe[Fl,Subscript[wwC, \[Phi]ud]]]/.listReempAllReal],True]/.listReempAllReal;
preCond2=DeleteCases[DeleteDuplicates[assumOpe[Fl,Subscript[wwC, u\[Phi]]]/.listReempAllReal],True];
preCond3=DeleteCases[DeleteDuplicates[Join[assumOpe[Fl,Subscript[wwC, ll]],assumOpe[Fl,Subscript[wwC, ee]],assumOpe[Fl,Subscript[wwC, le]]]/.listReempAllReal],True];
preCond4=DeleteCases[DeleteDuplicates[Join[assumOpe[Fl,Subscript[wwC, qq1]],assumOpe[Fl,Subscript[wwC, qq3]],assumOpe[Fl,Subscript[wwC, lq1]],assumOpe[Fl,Subscript[wwC, lq3]]]/.listReempAllReal],True];
preCond5=DeleteCases[DeleteDuplicates[Join[assumOpe[Fl,Subscript[wwC, dd]],assumOpe[Fl,Subscript[wwC, uu]],assumOpe[Fl,Subscript[wwC, ud1]],assumOpe[Fl,Subscript[wwC, ud8]],assumOpe[Fl,Subscript[wwC, eu]],assumOpe[Fl,Subscript[wwC, ed]]]/.listReempAllReal],True];
preCond6=DeleteCases[DeleteDuplicates[Join[assumOpe[Fl,Subscript[wwC, lu]],assumOpe[Fl,Subscript[wwC, ld]],assumOpe[Fl,Subscript[wwC, qe]],assumOpe[Fl,Subscript[wwC, qu1]],assumOpe[Fl,Subscript[wwC, qu8]],assumOpe[Fl,Subscript[wwC, qd1]],assumOpe[Fl,Subscript[wwC, qd8]]]/.listReempAllReal],True];
preCond6a=DeleteCases[DeleteDuplicates[Join[assumOpe[Fl,Subscript[wwC, lu]]]/.listReempAllReal],True];
preCond6b=DeleteCases[DeleteDuplicates[Join[assumOpe[Fl,Subscript[wwC, ld]]]/.listReempAllReal],True];
preCond6c=DeleteCases[DeleteDuplicates[Join[assumOpe[Fl,Subscript[wwC, qe]],assumOpe[Fl,Subscript[wwC, qu1]],assumOpe[Fl,Subscript[wwC, qu8]]]/.listReempAllReal],True];
preCond6d=DeleteCases[DeleteDuplicates[Join[assumOpe[Fl,Subscript[wwC, qd1]],assumOpe[Fl,Subscript[wwC, qd8]]]/.listReempAllReal],True];
preCond7=DeleteCases[DeleteDuplicates[Join[assumOpe[Fl,Subscript[wwC, ledq]],assumOpe[Fl,Subscript[wwC, quqd1]],assumOpe[Fl,Subscript[wwC, quqd8]],assumOpe[Fl,Subscript[wwC, lequ1]],assumOpe[Fl,Subscript[wwC, lequ3]]]/.listReempAllReal],True];
numCond=Length[preCond1]+Length[preCond2]+Length[preCond3]+Length[preCond4]+Length[preCond5]+Length[preCond6]+Length[preCond7];
If[numCond>0,
If[Length[preCond1]>0,
allSol=DeleteDuplicates[Quiet[Solve[preCond1]]];,
allSol={{}};];
(*//////Condition 2///////*)
If[Length[preCond2]>0&&Length[DeleteCases[DeleteDuplicates[Flatten[preCond2//.allSol]],True]]>0,
(*Print[allSol];*)
auxTab2=Table[DeleteCases[DeleteDuplicates[preCond2//.allSol[[indSol]]],True],{indSol,1,Length[allSol]}];
(*Print[auxTab2];*)
auxTab1=Table[If[auxTab2[[indSol]]!={},DeleteDuplicates[Quiet[Solve[preCond2//.allSol[[indSol]]]]],{{"AA"->"AA"}}],{indSol,1,Length[allSol]}];
(*Print[auxTab1];*)
(*Print[allSol];*)
For[indPreSol1=1,indPreSol1<=Length[allSol],indPreSol1++,
If[auxTab1[[indPreSol1]]!={{}},If[ToString[auxTab1[[indPreSol1]]]==ToString[{{"AA"->"AA"}}],
allSol[[indPreSol1]]=Piecewise[{{{allSol[[indPreSol1]]},ListQ[allSol[[indPreSol1]]]}},{{allSol[[indPreSol1]]}}];,
allSol[[indPreSol1]]=Table[Join[Piecewise[{{allSol[[indPreSol1]],ListQ[allSol[[indPreSol1]]]}},{allSol[[indPreSol1]]}],Piecewise[{{auxTab1[[indPreSol1,ind]],ListQ[auxTab1[[indPreSol1,ind]]]}},{auxTab1[[indPreSol1,ind]]}]],{ind,1,Length[auxTab1[[indPreSol1]]]}];],
allSol[[indPreSol1]]={{}};];];
(*Print[allSol];*)
allSol=DeleteCases[DeleteDuplicates[Flatten[allSol,1]],{}];];
(*Print["Ctrl1"];*)
(*Print[allSol];*)
(*///////Condition 7////////*)
If[Length[preCond7]>0&&Length[DeleteCases[DeleteDuplicates[Flatten[preCond7//.allSol]],True]]>0,
(*Print[allSol];*)
auxTab2=Table[DeleteCases[DeleteDuplicates[preCond7//.allSol[[indSol]]],True],{indSol,1,Length[allSol]}];
(*Print[auxTab2];*)
auxTab1=Table[If[auxTab2[[indSol]]!={},DeleteDuplicates[Quiet[Solve[preCond7//.allSol[[indSol]]]]],{{"AA"->"AA"}}],{indSol,1,Length[allSol]}];
(*Print[auxTab1];*)
(*Print[allSol];*)
For[indPreSol1=1,indPreSol1<=Length[allSol],indPreSol1++,
If[auxTab1[[indPreSol1]]!={{}},If[ToString[auxTab1[[indPreSol1]]]==ToString[{{"AA"->"AA"}}],
allSol[[indPreSol1]]=Piecewise[{{{allSol[[indPreSol1]]},ListQ[allSol[[indPreSol1]]]}},{{allSol[[indPreSol1]]}}];,
allSol[[indPreSol1]]=Table[Join[Piecewise[{{allSol[[indPreSol1]],ListQ[allSol[[indPreSol1]]]}},{allSol[[indPreSol1]]}],Piecewise[{{auxTab1[[indPreSol1,ind]],ListQ[auxTab1[[indPreSol1,ind]]]}},{auxTab1[[indPreSol1,ind]]}]],{ind,1,Length[auxTab1[[indPreSol1]]]}];],
allSol[[indPreSol1]]={{}};];];
(*Print[allSol];*)
allSol=DeleteCases[DeleteDuplicates[Flatten[allSol,1]],{}];];
(*Print["Ctrl2"];*)
(*Print[allSol];*)
(*///////Condition 4////////*)
If[Length[preCond4]>0&&Length[DeleteCases[DeleteDuplicates[Flatten[preCond4//.allSol]],True]]>0,
(*Print[allSol];*)
auxTab2=Table[DeleteCases[DeleteDuplicates[preCond4//.allSol[[indSol]]],True],{indSol,1,Length[allSol]}];
(*Print[auxTab2];*)
auxTab1=Table[If[auxTab2[[indSol]]!={},DeleteDuplicates[Quiet[Solve[preCond4//.allSol[[indSol]]]]],{{"AA"->"AA"}}],{indSol,1,Length[allSol]}];
(*Print[auxTab1];*)
(*Print[allSol];*)
For[indPreSol1=1,indPreSol1<=Length[allSol],indPreSol1++,
If[auxTab1[[indPreSol1]]!={{}},If[ToString[auxTab1[[indPreSol1]]]==ToString[{{"AA"->"AA"}}],
allSol[[indPreSol1]]=Piecewise[{{{allSol[[indPreSol1]]},ListQ[allSol[[indPreSol1]]]}},{{allSol[[indPreSol1]]}}];,
allSol[[indPreSol1]]=Table[Join[Piecewise[{{allSol[[indPreSol1]],ListQ[allSol[[indPreSol1]]]}},{allSol[[indPreSol1]]}],Piecewise[{{auxTab1[[indPreSol1,ind]],ListQ[auxTab1[[indPreSol1,ind]]]}},{auxTab1[[indPreSol1,ind]]}]],{ind,1,Length[auxTab1[[indPreSol1]]]}];],
allSol[[indPreSol1]]={{}};];];
(*Print[allSol];*)
allSol=DeleteCases[DeleteDuplicates[Flatten[allSol,1]],{}];];
(*Print["Ctrl4"];*)
(*Print[allSol];*)
(*//////Condition 6d///////*)
If[Length[preCond6d]>0&&Length[DeleteCases[DeleteDuplicates[Flatten[preCond6d//.allSol]],True]]>0,
(*Print[allSol];*)
auxTab2=Table[DeleteCases[DeleteDuplicates[preCond6d//.allSol[[indSol]]],True],{indSol,1,Length[allSol]}];
(*Print[auxTab2];*)
auxTab1=Table[If[auxTab2[[indSol]]!={},DeleteDuplicates[Quiet[Solve[preCond6d//.allSol[[indSol]]]]],{{"AA"->"AA"}}],{indSol,1,Length[allSol]}];
(*Print[auxTab1];*)
(*Print[allSol];*)
For[indPreSol1=1,indPreSol1<=Length[allSol],indPreSol1++,
If[auxTab1[[indPreSol1]]!={{}},
If[ToString[auxTab1[[indPreSol1]]]==ToString[{{"AA"->"AA"}}],
allSol[[indPreSol1]]=Piecewise[{{{allSol[[indPreSol1]]},ListQ[allSol[[indPreSol1]]]}},{{allSol[[indPreSol1]]}}];,
allSol[[indPreSol1]]=Table[Join[Piecewise[{{allSol[[indPreSol1]],ListQ[allSol[[indPreSol1]]]}},{allSol[[indPreSol1]]}],Piecewise[{{auxTab1[[indPreSol1,ind]],ListQ[auxTab1[[indPreSol1,ind]]]}},{auxTab1[[indPreSol1,ind]]}]],{ind,1,Length[auxTab1[[indPreSol1]]]}];],
allSol[[indPreSol1]]={{}};];];
(*Print[allSol];*)
allSol=DeleteCases[DeleteDuplicates[Flatten[allSol,1]],{}];];
(*Print["Ctrl3d"];*)
(*Print[allSol];*)
(*//////Condition 6c///////*)
If[Length[preCond6c]>0&&Length[DeleteCases[DeleteDuplicates[Flatten[preCond6c//.allSol]],True]]>0,
(*Print[allSol];*)
auxTab2=Table[DeleteCases[DeleteDuplicates[preCond6c//.allSol[[indSol]]],True],{indSol,1,Length[allSol]}];
(*Print[auxTab2];*)
auxTab1=Table[If[auxTab2[[indSol]]!={},DeleteDuplicates[Quiet[Solve[preCond6c//.allSol[[indSol]]]]],{{"AA"->"AA"}}],{indSol,1,Length[allSol]}];
(*Print[auxTab1];*)
(*Print[allSol];*)
For[indPreSol1=1,indPreSol1<=Length[allSol],indPreSol1++,
If[auxTab1[[indPreSol1]]!={{}},If[ToString[auxTab1[[indPreSol1]]]==ToString[{{"AA"->"AA"}}],
allSol[[indPreSol1]]=Piecewise[{{{allSol[[indPreSol1]]},ListQ[allSol[[indPreSol1]]]}},{{allSol[[indPreSol1]]}}];,
allSol[[indPreSol1]]=Table[Join[Piecewise[{{allSol[[indPreSol1]],ListQ[allSol[[indPreSol1]]]}},{allSol[[indPreSol1]]}],Piecewise[{{auxTab1[[indPreSol1,ind]],ListQ[auxTab1[[indPreSol1,ind]]]}},{auxTab1[[indPreSol1,ind]]}]],{ind,1,Length[auxTab1[[indPreSol1]]]}];],
allSol[[indPreSol1]]={{}};];];
(*Print[allSol];*)
allSol=DeleteCases[DeleteDuplicates[Flatten[allSol,1]],{}];];
(*Print["Ctrl3c"];*)
(*Print[allSol];*)
(*//////Condition 6b///////*)
If[Length[preCond6b]>0&&Length[DeleteCases[DeleteDuplicates[Flatten[preCond6b//.allSol]],True]]>0,
(*Print[allSol];*)
auxTab2=Table[DeleteCases[DeleteDuplicates[preCond6b//.allSol[[indSol]]],True],{indSol,1,Length[allSol]}];
(*Print[auxTab2];*)
auxTab1=Table[If[auxTab2[[indSol]]!={},DeleteDuplicates[Quiet[Solve[preCond6b//.allSol[[indSol]]]]],{{"AA"->"AA"}}],{indSol,1,Length[allSol]}];
(*Print[auxTab1];*)
(*Print[allSol];*)
For[indPreSol1=1,indPreSol1<=Length[allSol],indPreSol1++,
If[auxTab1[[indPreSol1]]!={{}},If[ToString[auxTab1[[indPreSol1]]]==ToString[{{"AA"->"AA"}}],
allSol[[indPreSol1]]=Piecewise[{{{allSol[[indPreSol1]]},ListQ[allSol[[indPreSol1]]]}},{{allSol[[indPreSol1]]}}];,
allSol[[indPreSol1]]=Table[Join[Piecewise[{{allSol[[indPreSol1]],ListQ[allSol[[indPreSol1]]]}},{allSol[[indPreSol1]]}],Piecewise[{{auxTab1[[indPreSol1,ind]],ListQ[auxTab1[[indPreSol1,ind]]]}},{auxTab1[[indPreSol1,ind]]}]],{ind,1,Length[auxTab1[[indPreSol1]]]}];],
allSol[[indPreSol1]]={{}};];];
(*Print[allSol];*)
allSol=DeleteCases[DeleteDuplicates[Flatten[allSol,1]],{}];];
(*Print["Ctrl3b"];*)
(*Print[allSol];*)
(*//////Condition 6a///////*)
If[Length[preCond6a]>0&&Length[DeleteCases[DeleteDuplicates[Flatten[preCond6a//.allSol]],True]]>0,
(*Print[allSol];*)
auxTab2=Table[DeleteCases[DeleteDuplicates[preCond6a//.allSol[[indSol]]],True],{indSol,1,Length[allSol]}];
(*Print[auxTab2];*)
auxTab1=Table[If[auxTab2[[indSol]]!={},DeleteDuplicates[Quiet[Solve[preCond6a//.allSol[[indSol]]]]],{{"AA"->"AA"}}],{indSol,1,Length[allSol]}];
(*Print[auxTab1];*)
(*Print[allSol];*)
For[indPreSol1=1,indPreSol1<=Length[allSol],indPreSol1++,
If[auxTab1[[indPreSol1]]!={{}},If[ToString[auxTab1[[indPreSol1]]]==ToString[{{"AA"->"AA"}}],
allSol[[indPreSol1]]=Piecewise[{{{allSol[[indPreSol1]]},ListQ[allSol[[indPreSol1]]]}},{{allSol[[indPreSol1]]}}];,
allSol[[indPreSol1]]=Table[Join[Piecewise[{{allSol[[indPreSol1]],ListQ[allSol[[indPreSol1]]]}},{allSol[[indPreSol1]]}],Piecewise[{{auxTab1[[indPreSol1,ind]],ListQ[auxTab1[[indPreSol1,ind]]]}},{auxTab1[[indPreSol1,ind]]}]],{ind,1,Length[auxTab1[[indPreSol1]]]}];],
allSol[[indPreSol1]]={{}};];];
(*Print[allSol];*)
allSol=DeleteCases[DeleteDuplicates[Flatten[allSol,1]],{}];];
(*Print["Ctrl3a"];*)
(*Print[allSol];*)
(*//////Condition 5///////*)
If[Length[preCond5]>0&&Length[DeleteCases[DeleteDuplicates[Flatten[preCond5/.allSol]],True]]>0,
(*Print[allSol];*)
auxTab2=Table[DeleteCases[DeleteDuplicates[preCond5//.allSol[[indSol]]],True],{indSol,1,Length[allSol]}];
(*Print[auxTab2];*)
auxTab1=Table[If[auxTab2[[indSol]]!={},DeleteDuplicates[Quiet[Solve[preCond5//.allSol[[indSol]]]]],{{"AA"->"AA"}}],{indSol,1,Length[allSol]}];
(*Print[auxTab1];*)
(*Print[allSol];*)
For[indPreSol1=1,indPreSol1<=Length[allSol],indPreSol1++,
If[auxTab1[[indPreSol1]]!={{}},If[ToString[auxTab1[[indPreSol1]]]==ToString[{{"AA"->"AA"}}],
allSol[[indPreSol1]]=Piecewise[{{{allSol[[indPreSol1]]},ListQ[allSol[[indPreSol1]]]}},{{allSol[[indPreSol1]]}}];,
allSol[[indPreSol1]]=Table[Join[Piecewise[{{allSol[[indPreSol1]],ListQ[allSol[[indPreSol1]]]}},{allSol[[indPreSol1]]}],Piecewise[{{auxTab1[[indPreSol1,ind]],ListQ[auxTab1[[indPreSol1,ind]]]}},{auxTab1[[indPreSol1,ind]]}]],{ind,1,Length[auxTab1[[indPreSol1]]]}];],
allSol[[indPreSol1]]={{}};];];
(*Print[allSol];*)
allSol=DeleteCases[DeleteDuplicates[Flatten[allSol,1]],{}];];
(*Print["Ctrl5"];*)
(*Print[allSol];*)
(*//////Condition 3///////*)
If[Length[preCond3]>0&&Length[DeleteCases[DeleteDuplicates[Flatten[preCond3//.allSol]],True]]>0,
(*Print[allSol];*)
auxTab2=Table[DeleteCases[DeleteDuplicates[preCond3//.allSol[[indSol]]],True],{indSol,1,Length[allSol]}];
(*Print[auxTab2];*)
auxTab1=Table[If[auxTab2[[indSol]]!={},DeleteDuplicates[Quiet[Solve[preCond3//.allSol[[indSol]]]]],{{"AA"->"AA"}}],{indSol,1,Length[allSol]}];
(*Print[auxTab1];*)
(*Print[allSol];*)
For[indPreSol1=1,indPreSol1<=Length[allSol],indPreSol1++,
If[auxTab1[[indPreSol1]]!={{}},If[ToString[auxTab1[[indPreSol1]]]==ToString[{{"AA"->"AA"}}],
allSol[[indPreSol1]]=Piecewise[{{{allSol[[indPreSol1]]},ListQ[allSol[[indPreSol1]]]}},{{allSol[[indPreSol1]]}}];,
allSol[[indPreSol1]]=Table[Join[Piecewise[{{allSol[[indPreSol1]],ListQ[allSol[[indPreSol1]]]}},{allSol[[indPreSol1]]}],Piecewise[{{auxTab1[[indPreSol1,ind]],ListQ[auxTab1[[indPreSol1,ind]]]}},{auxTab1[[indPreSol1,ind]]}]],{ind,1,Length[auxTab1[[indPreSol1]]]}];],
allSol[[indPreSol1]]={{}};];];
(*Print[allSol];*)
allSol=DeleteCases[DeleteDuplicates[Flatten[allSol,1]],{}];];
(*Print["Ctrl6"];*)
(*Print[allSol];*)
Print["Length of the set of non-trivial solutions: "<>ToString[Length[allSol]]];
allSol,
Print["All conditions satisfied trivially."];{{AA->AA}}]];
onePartExtDictioSMEFTsimBasis:=Block[{indPart,solFlaSym,ret,conseqFlaSym,maxConseq,flag,mmEFTdicto,solFlaSymOut},ret={{"Particle","Dictionary"}};
For[indPart=1,indPart<=cutOffPart,indPart++,
If[indPart==27,Continue[];];
setAllToZero[all];n[nameListAll[[indPart]]]=1;listM[nameListAll[[indPart]]]={m};allocateAllMasses;
solFlaSym=superSolAsummOpe[flavourSym];
flag="YES";
If[solFlaSym=={},solFlaSym={vanishAllUVCouplings};flag="NO"];
If[Length[solFlaSym]>1,conseqFlaSym=Table[Length[DeleteCases[DeleteCases[Chop[ReleaseHold[requiredWCsmeftsimBasis[[2;;-1,2]]]/.listReempAllReal/.solFlaSym[[i]]/.listReempAllReal],0.],0]],{i,1,Length[solFlaSym]}];
maxConseq=Position[conseqFlaSym,Max[conseqFlaSym]];
If[Length[maxConseq],Print["There are more than 1 solutions to the Flavour assumptions that maximizes the number of non-vanishing WCs."];];
Print[maxConseq[[1,1]]];
Print[solFlaSym];
solFlaSym={solFlaSym[[maxConseq[[1,1]]]]};
Print[solFlaSym];
];
mmEFTdicto={{},{},ParallelTable[mmEFTout[[1,j,1]]->ComplexExpand[(ReleaseHold[mmEFTout[[1,j,2]]])],{j,1,Length[mmEFTout[[1]]]}]/.listReempAllReal/.allUVCouplingsSimplified/.{ySM[u][i_,j_]:>yu[i,j],ySM[d][i_,j_]:>yd[i,j],ySM[e][i_,j_]:>yl[i,j]},{g1 -> g1, g2 -> g2, g3 -> g3}};
ret=Join[ret,{{nameListAll[[indPart]],mmEFTdicto,flag}}];
Print[indPart];
solFlaSymOut=solFlaSym[[1]]/.allUVCouplingsSimplified;
mmEFTdicto>>(Directory[]<>"/Matching_Result_Tree_Level_Mod_"<>ToString[indPart+1]<>"_"<>DateString[{"Day","_","Month","_","Year"}]<>".dat");
solFlaSymOut>>(Directory[]<>"/Flavour_Sym_Cond_SMEFiT_Tree_Level_Mod_"<>ToString[indPart+1]<>"_"<>DateString[{"Day","_","Month","_","Year"}]<>".dat")
];];
onePartExtDictioSMEFTsimBasis




