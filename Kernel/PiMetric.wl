BeginPackage["Tiago`PiMetric`"]

PiMetric::usage = "PiMetric[PiVector, Method] Evaluates the associated metric of the Pi Group."


Begin["`Private`"]

Options[PiMetric] = {
    Method -> "L1"
};


PiMetric[piVector_?ArrayQ, opts:OptionsPattern[]] := Module[
    {method, norm, vector},

    If[!VectorQ[piVector] && !MatrixQ[piVector] && !ArrayQ[piVector, 3],
        Message[PiMetric::invdata];
        Return[$Failed]
    ];

    method = OptionValue[PiMetric, {opts}, Method];
    norm = PiMetricEvaluate[method, piVector];

    Total[#]&/@norm
]


PiMetricEvaluate["L1", vector_?VectorQ] := (
    Norm[vector, 1]
)
PiMetricEvaluate["L2", vector_?VectorQ] := (
    Norm[vector, 2]
)
PiMetricEvaluate[method_String, matrix_?ArrayQ] := (
    PiMetricEvaluate[method, #]& /@ matrix
)


End[]

EndPackage[]