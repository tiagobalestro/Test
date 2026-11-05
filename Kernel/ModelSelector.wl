(* ::Package:: *)

BeginPackage["Tiago`ModelSelector`",  {"Tiago`PiMetric`", "Tiago`PiModel`", "Tiago`ModelEvaluate`", "Tiago`SINDy`"}]

ModelSelector::usage = "ModelSelector[] select the best model accordingly to a Selection Criteria."
Model::usage = ""

Begin["`Private`"]

ModelSelector[data_, hData_, varsAssoc_] := Module[
    {modelo, PiGroupList, Piout, Piin, Pi0, Pi1, Pi2,
     fun, thr, thresholds, fullDataModel, SINDySolution},


    modelo = PiModel[varsAssoc, "h"];
    PiGroupList = modelo["\[CapitalPi]_in"];

    Piout = PiDataEvaluate[modelo["\[CapitalPi]_out"], data];
    Piin  = PiDataEvaluate[modelo["\[CapitalPi]_in"], data];
    (* Echo[Piout,"Piout: "]; *)
    Pi0 = Piout[[24, 1]]*Flatten[hData];
    
   

    fun = {HoldForm[#1/#1], #1, #2, #1 #2, #1^2, #2^2, (#1 #2)^2, #1^3, #2^3} &;
      (*fun = {HoldForm[#1/#1], #1, #2, Sin[#1], Sin[#2], Cos[#1], Cos[#2]} &;*)
    thresholds = 10^Subdivide[Log10[0.001], Log10[2.5], 14];
    (* thresholds = Range[1, 15]/10.; *)

    fullDataModel = Flatten @ Table[
        With[{
            Pi1 = Piin[[i, 1]],
            Pi2 = Piin[[i, 2]],
            currentPiGroup = PiGroupList[[i]], (* Aqui o grupo acompanha o \[IAcute]ndice do Piin *)
            
            SINDySolution = SINDy[
                {Pi0, Piin[[i, 1]], Piin[[i, 2]]}, 
                fun,
                "Threshold" -> thr, 
                "Alpha" -> 0.0001, 
                Method -> {"STRidge", "OLS"}
            ]
        },
        
        <|
            "Threshold" -> thr,
            "BIC" -> ModelEvaluate[SINDySolution, currentPiGroup, 200], (* Grupo injetado corretamente *)
            "Fun" -> SINDySolution["fun"],
            "Xi" -> SINDySolution["\[CapitalXi]"],
            "Pi" -> AlgebraicForm[currentPiGroup, ToExpression[Keys[varsAssoc]][[2;;]]]
        |>
        ],
        
        {i, Length[Piin]}, (* Iterador de \[IAcute]ndice comum a ambas as listas *)
        {thr, thresholds}
    ];

    Model[fullDataModel]
]

Model[data_List]["BestBICPerThreshold"] := 
    Values @ Map[ First @* MinimalBy[Key["BIC"]], 
    GroupBy[data, Key["Threshold"]]
]

(* Model /: Format[Model[list_List]] := Model[
    "Model"
] *)

End[]

EndPackage[]
