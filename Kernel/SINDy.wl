BeginPackage["Tiago`SINDy`", {"Tiago`STRidge`", "Tiago`STLSq`", "Tiago`ThetaMatrix`"}]


SINDy::usage        = "SINDy[{Piout,Pi1,Pi2},fun_Library] roda a identifica\[CCedilla]\[ATilde]o esparsa usando STLSq."
SINDyModel::usage   = "SINDyModel[] is a structure of the SINDy model sparse regression."


Begin["`Private`"]

Options[SINDy] = {
    Method -> "STRidge",
    "Threshold" -> 0.1,
    "MaxIterations" -> 10,
    "Alpha" -> 0.1
};


SINDy[dataList_?ListQ, fLibrary_, OptionsPattern[]] := Module[
    {sol, method, lambda, alpha},
    
    method = OptionValue[Method];
    lambda = OptionValue["Threshold"];
    alpha = OptionValue["Alpha"];

    sol = SINDySolve[dataList, fLibrary, method, lambda, alpha];

    SINDyModel[sol]
]

SINDyModel[assoc_Association][prop_String] := Lookup[assoc, prop, $Failed];
SINDyModel[assoc_Association]["Properties"] := Keys[assoc];

SINDyModel /: MakeBoxes[obj : SINDyModel[assoc_Association], fmt_] := Module[
    {icon, visivel, oculto},
    
    (* Ícone exibido no canto esquerdo da caixinha *)
    (* icon = Graphics[{RGBColor[0.2, 0.5, 0.8], Disk[]}, ImageSize -> 20]; *)
    
    (* Informações que ficam sempre visíveis *)
    visivel = {
      BoxForm`SummaryItem[{"Function: ", assoc["fun"]}]
    };
    
    (* Informações que aparecem ao clicar na setinha '+' *)
    oculto = {
      BoxForm`SummaryItem[{"Xi: ", assoc["\[CapitalXi]"]}],
      BoxForm`SummaryItem[{"iteration: ", assoc["iteration"]}],
      BoxForm`SummaryItem[{"R2: ", assoc["R2"]}]
    };
    
    (* Construtor padrão da interface da Wolfram *)
    BoxForm`ArrangeSummaryBox[
      SINDyModel, (* Título do cabeçalho *)
      obj,          (* Objeto completo *)
      None,         (* Ícone *)
      visivel,      (* Linhas visíveis *)
      oculto,       (* Linhas expansíveis *)
      fmt
    ]
];


(*SINDy Solvers Available*)

(*STRidge*)
SINDySolve[dataList_?ListQ, fLibrary_, "STRidge", lambda_, alpha_] := Module[
        {ThetaNormal, ThetaNorm, XNormal, XNorm, XiNormal, iter, Xi, Xbar, SQR, SQT, Theta, R2},

		{ThetaNormal, ThetaNorm} = ThetaMatrix[dataList, fLibrary];

		{XNormal, XNorm} = XVector[dataList];

		{XiNormal, iter} = STRidge[ThetaNormal, XNormal, lambda, alpha]; (*STRidge Solver*)
        
        Xi = XiNormal * (XNorm / ThetaNorm);

        <|
            "\[CapitalXi]" ->Xi,
            "iteration" -> iter,
            "\[Theta] norm" -> ThetaNorm,
            "X norm" -> XNorm,
            "fun" -> Round[Xi,0.01].Flatten[fLibrary]@@{Subscript["\[CapitalPi]", 1],Subscript["\[CapitalPi]", 2]},
            "R2" -> R2
        |>
    ]

(*OLS*)
SINDySolve[dataList_?ListQ, fLibrary_, "OLS"] := Module[
    {Xi},

    (* {ThetaNormal, ThetaNorm} = ThetaMatrix[dataList, reducedfLibrary]; *)
    {ThetaNormal, ThetaNorm} = ThetaMatrix[dataList, fLibrary];
    {XNormal, XNorm} = XVector[dataList];
        
    XiNormal = LeastSquares[ThetaNormal, XNormal];
         
    Xi = XiNormal * (XNorm / ThetaNorm);
    Theta = Transpose[Transpose[ThetaNormal] * ThetaNorm];

    (* MSE *)
    PiPredicted = Theta . Xi;
    PiData = dataList[[1]];
    n = Length[PiData];
    MSE = (1 / n) * Total[(PiData - PiPredicted)^2];

    <|
        "OLSXi" -> Xi,
        "MSE" -> MSE
    |>
]


(*STRidge followed by OLS*)
SINDySolve[dataList_?ListQ, fLibrary_, {"STRidge", "OLS"}, lambda_, alpha_] := Module[
    {Xi},

    Xi = SINDySolve[dataList, fLibrary, "STRidge", lambda, alpha]["\[CapitalXi]"];

    slotRef = UnitStep[Abs[Xi] - 10^-6];
    reducedfLibrary = Evaluate[Pick[fLibrary[[1]], slotRef, 1]]&;  

    XiOLS = SINDySolve[dataList, reducedfLibrary, "OLS"]["OLSXi"];
    MSE = SINDySolve[dataList, reducedfLibrary, "OLS"]["MSE"];
        
    <|
        "\[CapitalXi]" ->Xi,
        "fun" -> Round[Xi,0.01].Flatten[fLibrary]@@{Subscript["\[CapitalPi]", 1],Subscript["\[CapitalPi]", 2]},
        "R2" -> R2,
        "XiOLS" -> XiOLS,
        "funOLS" -> Round[XiOLS,0.01].Flatten[reducedfLibrary]@@{Subscript["\[CapitalPi]", 1],Subscript["\[CapitalPi]", 2]},
        "MSE" -> MSE
    |>
]


End[]

EndPackage[]