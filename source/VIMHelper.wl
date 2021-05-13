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

ShowPlot[plot_] := Module[{randomFileName, filePath},
    (*randomFileName = RandomFileName[];*)
    randomFileName = "wolframPlot.pdf";
    filePath = FileNameJoin[{"/tmp",randomFileName}];
    Export[filePath, plot];
    Run["open "<>filePath];
];


Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["VIMHelper`*"], Head[#] === Symbol &]];

End[];
EndPackage[];

