BeginPackage["Tiago`STLSq`"]

STLSq::usage = "STLSq[\[CapitalTheta],X,\[Lambda]] solves the STLSq method."


Begin["`Private`"]

Options[STLSq] = {
    MaxIterations -> 10, 
    Tolerance -> 0.000000001
};


STLSq[Theta_?MatrixQ, Y_?VectorQ, lambda_?NumericQ, OptionsPattern[]] :=
    Module[
    {XiInitial, maxIteration, tol, XiNew},
        
    XiInitial = LeastSquares[Theta, Y];
    maxIteration = OptionValue[MaxIterations];
    tol = OptionValue[Tolerance];
    XiNew = FixedPoint[
    STLSqStep[Theta, Y, lambda],
    XiInitial,
    maxIteration,
    SameTest -> (Max[Abs[#1 - #2]] < tol &)
    (*SameTest -> (Max[Abs[XiInitial - XiNew]] < tol)*)];
         
    XiNew
]


STLSqStep[Theta_?MatrixQ, Y_?VectorQ, lambda_][Xi_] := Module[
    {Xiunitized, activeIndexes, ThetaSub, Xisub, Xiaux, Xilambda},

    (*Xi0=LeastSquares[Theta,Y];*)
    Xilambda = Threshold[Xi, lambda];
    (**)
    Xiunitized = Unitize[Xilambda];
    activeIndexes = Pick[Range[Length[Xiunitized]], Xiunitized, 1];
    (**)
    If[Length[activeIndexes] == 0,
        Return[Xilambda]
    ];
    ThetaSub = Theta[[All, activeIndexes]];
    Xisub = LeastSquares[ThetaSub, Y];
    Xiaux = Xilambda;
    Xiaux[[activeIndexes]] = Xisub;

    Xiaux
]

End[]

EndPackage[]