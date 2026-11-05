BeginPackage["Tiago`PiModel`"]


PiModel::usage = 		"PiModel[varsExp, freeVar, gamma matrix] finds the Pi Group in vector form."
GammaMatrix::usage = 	"GammaMatrix[{m, n}, Grid -> {}] gives the gamma constant up to Grid value."
PiDataEvaluate::usage = "PiDataEvaluate[PiExpVector, data] evaluates data accordingly the Pi Group."
AlgebraicForm::usage = 	"AlgebraicForm[vectorForm,varsList] gives the algebric form of a Pi Group."
PiSetModel::usage = ""

Begin["`Private`"]

Options[GammaMatrix] = {
	"Grid" -> {0, 1}
};


PiModel[varsAssoc_?AssociationQ, goalVar_?StringQ, OptionsPattern[]] := Module[
	{goalVarVector, reducVarsAssoc, dimMatrix, wp, nullSpace, n, gammaIn, gammaOut, PiOut, PiIn},

	goalVarVector = varsAssoc[goalVar];					(*Get the vector of the goal variable*)
	reducVarsAssoc = KeyDrop[varsAssoc, goalVar];		(*Drop the goal variable of the remaining variables*)

	dimMatrix = Transpose[Values[reducVarsAssoc]];		(*Build the dimencional Matrix*)

	wp = LinearSolve[dimMatrix, -goalVarVector];		(*Find a particular solutution that solves the sustem M.wp = -a*)

	nullSpace = NullSpace[dimMatrix]//LatticeReduce;
	n = Length[nullSpace];	

	(*Evaluates Pi out*)
	gammaOut = GammaMatrix[{1, n}, "Grid"-> {-1, 0, 1}];

	PiOut = Map[ConstantArray[wp, 1] + #.nullSpace&, gammaOut];

	(*Evaluates Pi_inlet*)
	gammaIn = GammaMatrix[{n-1, n}, "Grid" -> {-1, 0, 1}];
	PiIn = Map[#.nullSpace&, gammaIn];

	(*Return Result*)
	PiSetModel[
		<|
			"\[CapitalPi]_out" -> PiOut,
			"\[CapitalPi]_in" -> PiIn
		|>
	]
]

PiSetModel /: Format[PiSetModel[assoc_Association]] := PiSetModel[
	"< \[CapitalPi]_out: " <> ToString[Length[assoc[[1]]]] <> "itens, \[CapitalPi]_in: " <> ToString[Length[assoc[[2]]]] <> " candidates>"
]

PiSetModel[assoc_Association][key_String] := assoc[key]


GammaMatrix[{m_?IntegerQ, n_?IntegerQ}, OptionsPattern[]] := Module[
	{g, vetores, vetoresLimpos, resultado},

    g = OptionValue["Grid"];

    vetores = Tuples[g, n];
    vetoresLimpos = DeleteCases[vetores, ConstantArray[0, n]];
    (* resultado = DeleteDuplicates[vetoresLimpos, MatrixRank[{#1, #2
        }] == 1&]; *)
	
    Subsets[vetoresLimpos, {m}]
]


PiDataEvaluate[PiExpVector_?ListQ, data_?ListQ] /; ArrayQ[PiExpVector,3, NumericQ] := Module[
	{dataT},
    dataT = Transpose[data];
    Map[Times @@ (dataT^#)&, PiExpVector, {2}]
]


AlgebraicForm[vectorForm_?VectorQ,varsList_?VectorQ] := (
	Times @@ (varsList^vectorForm)
)
AlgebraicForm[vectorForm_?MatrixQ,varsList_?VectorQ] := (
	Map[(Times @@ (varsList^#))&, vectorForm, {1}]
)


End[]

EndPackage[]