BeginPackage["Tiago`ModelEvaluate`",  {"Tiago`PiMetric`", "Tiago`SINDy`"}]

ModelEvaluate::usage = "ModelEvaluate[] select the best model accordingly to a Selection Criteria."


Begin["`Private`"]

Options[ModelEvaluate] = {
    "SelectionCriteria" -> "BIC",
    "PiPenalty" -> 1
};


(* ModelEvaluate[SIEq_?ListQ, PiGroupList_, n_] := Module[
    {l, XiList, kList, MSEList, CPiList, alpha, BIC},

    l = Length[SIEq];

    XiList = SIEq[[#]]["XiOLS"] & /@ Range[1, l];
    kList = Length/@XiList;
    MSEList = SIEq[[#]]["MSE"] & /@ Range[1, l];
    CPiList = PiMetric[PiGroupList, Method -> "L1"];

    alpha = 1;
    BIC = n*Log[MSEList] + (kList + alpha*CPiList)*Log[n];

    BIC
] *)


ModelEvaluate[SIEq_SINDyModel, PiGroup_, n_, OptionsPattern[]] := Module[
    {Xi, k, MSE, CPi, alpha, BIC},

    alpha = OptionValue["PiPenalty"];

    Xi = SIEq["XiOLS"];
    k = Length[Xi];
    MSE = SIEq["MSE"];
    CPi = PiMetric[PiGroup, Method -> "L1"]//Total;


    BIC = n*Log[MSE] + (k + alpha*CPi)*Log[n];

    BIC
]



	
End[]

EndPackage[]