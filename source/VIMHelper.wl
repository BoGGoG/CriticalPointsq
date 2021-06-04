BeginPackage["VIMHelper`"]

Unprotect["VIMHelper`*"];
ClearAll["VIMHelper`*", "VIMHelper`Private`*"];

ShowPlot::usage = "ShowPlot[plot] opens plot in zathura viewer.
For this, the plot gets saved as pdf into the /tmp/ directory.";

Begin["`Private`"];

RandomFileName[length_:50, extension_:"pdf"] := Block[{name},
    name = ResourceFunction["RandomString"][length];
    name<>"."<>extension
];

ShowPlot[plot_] := Module[{randomFileName, randomFileNum, filePath},
    (*randomFileName = RandomFileName[];*)
    randomFileNum = StringJoin @@ Map[ToString]@
        Array[RandomInteger[{0, 9}] &, {10}]

    randomFileName = StringJoin["wolframPlot",randomFileNum,".pdf"];
    filePath = FileNameJoin[{$TemporaryDirectory,randomFileName}];
    Export[filePath, plot];

    Print["Opening Plot Image from " <> filePath]
    Run["open "<>filePath];
];


Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["VIMHelper`*"], Head[#] === Symbol &]];

End[];
EndPackage[];

