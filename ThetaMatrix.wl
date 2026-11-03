BeginPackage["Tiago`ThetaMatrix`"]

ThetaMatrix::usage	= "ThetaMatrix[dataList,fun_library] builds the Theta Matrix"
XVector::usage		= "XVector[list] normalizes the X vector with L_inf norm."


Begin["`Private`"]


ThetaMatrix[dataList_?ListQ,fLibrary_]:= Module[
		{dataSetLength, Pin,ThetaT},

		dataSetLength = Length[dataList];

		Pin = dataList[[2;;]];

		ThetaT = ReleaseHold[fLibrary]@@Pin;
		
		columnScale = Max[Abs[#]] & /@ ThetaT;
		columnScale = Replace[columnScale, 0 | 0. :> 1.0, {1}];
		ThetaTNormal = ThetaT / columnScale;

		{Transpose[ThetaTNormal], columnScale} 		
	]


XVector[dataList_?ListQ] := Module[
	{X, XNorm, XNormalized},

	X = dataList[[1]];

	XNorm = Max[Abs[#]] & @ X;
	XNormalized = X / XNorm;

	{XNormalized, XNorm}
]


End[]

EndPackage[]