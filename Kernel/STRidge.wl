BeginPackage["Tiago`STRidge`"]


STRidge::usage              = "STRidge[\[CapitalTheta],X,\[Lambda],\[Alpha]] solves the STRidge method."
(*RidgeRegression::usage    = "RidgeRegression[Theta, Y, alpha] calcula a penalizacao."*)


Begin["`Private`"]

Options[STRidge] = {
    MaxIterations -> 15,
    Tolerance -> 0.00001
};


STRidge[Theta_?MatrixQ, Y_?VectorQ, lambda_?NumericQ, alpha_?NumericQ, OptionsPattern[]] := Module[
    {XiInitial, maxIteration, tol, XiNew},

    maxIteration = OptionValue[MaxIterations];
    tol = OptionValue[Tolerance];
        
    XiInitial = RidgeRegression[Theta, Y, alpha];
        
    XiNew = FixedPointList[
        STRidgeStep[Theta, Y, lambda, alpha],
        XiInitial,
        maxIteration,
        (*SameTest -> (Max[Abs[#1 - #2]] < tol &)*)
        (*SameTest -> (Max[Abs[XiInitial - XiNew]] < tol)*)
        SameTest -> (Max[Abs[#1 - #2]] < tol &)
    ];
            
    {Last[XiNew], Length[XiNew]}
]


STRidgeStep[Theta_?MatrixQ, Y_?VectorQ, lambda_, alpha_][Xi_] :=
    Module[
        {Xiunitized, activeIndexes, ThetaSub, Xisub, Xiaux, Xilambda},
            
        (*Xi0=LeastSquares[Theta,Y];*)
        Xilambda = Threshold[Xi, lambda];

        (**)
        Xiunitized = Unitize[Xilambda];
        activeIndexes = Pick[Range[Length[Xiunitized]], Xiunitized, 1
            ];
        (**)
        If[Length[activeIndexes] == 0,
            Return[Xilambda]
        ];
        ThetaSub = Theta[[All, activeIndexes]];

        Xisub = RidgeRegression[ThetaSub, Y, alpha];
        
        Xiaux = Xilambda;
        Xiaux[[activeIndexes]] = Xisub;

        Xiaux
    ]
	

RidgeRegression[Theta_?MatrixQ, Y_?VectorQ, alpha_?NumericQ] := Module[
    {A, b, m, n},
    {m, n} = Dimensions[Theta];

    A = Transpose[Theta] . Theta + alpha * IdentityMatrix[n];
    b = Transpose[Theta] . Y;

    LinearSolve[A, b]
]


End[]

EndPackage[]