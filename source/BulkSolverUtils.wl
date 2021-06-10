(*This code is meant to provide bulk function to find QNMs/Critical in bulk.*)


(* ::Package:: *)

BeginPackage["BulkSolverUtils`"];

Unprotect["BulkSolverUtils`*"];
ClearAll["BulkSolverUtils`*", "BulkSolverUtils`Private`*"];


(* Begin Documentation/Declaration - I recommend to edit documentation via 
the GUI if possible.*)

solverTable::usage="solverTable[solver, {solverArgs}, elementFunc, tableFunc, {i, i0, if, di}, {j, j0, jf, dj},....] = runs solver[solverArgs] for give i,j,... for a given range. For every output of solver[solverArgs] the output is acted upon by elementFunc as elementFunc[solver[solverArgs,{i,i0..}, {j,j0..}]. tableFunc acts on the whole output table.";

funcTable::usage="funcTable[solver, {solverArgs}, , {i, i0, if, di}, {j, j0, jf, dj},...] = Solves using solver over range";

qnmTable::usage="qnmTable[solver, {solverArg1, solverArg2,...}, tableArgs] = Is just like solverTable, but elementFunc := Table[{Sequence@@({##2}[[;;,1]]), Re[mode], Im[mode]}, {mode, #1}]& and elementFunc:=Flatten[#1, numTabArgs]&";

Begin["`Private`"];
(*End of Package*)

SyntaxInformation[solverTable] = {
    "ArgumentsPattern" -> {_, {___},_,_,_, {_, _, _., _.}..},
    "LocalVariables" -> {"Table", {5,Infinity}}
    } 

solverTable[solver_, {solverArgs__}, elFunc_, tableFunc_, tableArgs__:{_Symbol, _, _., _.}] := 
Module[{numTabArgs = Length@List[tableArgs]},
    tableFunc[
        Apply[
            elFunc,
            Table[{solver[solverArgs],tableArgs}, tableArgs],
        {numTabArgs}]
    ]
]


SyntaxInformation[funcTable] = {
    "ArgumentsPattern" -> {_, {___}, {_, _, _., _.}..},
    "LocalVariables" -> {"Table", {3,Infinity}}
    } 

funcTable[solver_, {solverArgs__}, tableArgs__:{_Symbol, _, _., _.}] := solverTable[
solver, {solverArgs}, #1&, Identity, tableArgs]


SyntaxInformation[qnmTable] = {
    "ArgumentsPattern" -> {_, {___}, {_, _, _., _.}..},
    "LocalVariables" -> {"Table", {3,Infinity}}
    } 

qnmTable[solver_, {solverArgs__}, tableArgs__:{_Symbol, _, _., _.}] := Module[
    {numTabArgs = Length@List[tableArgs],mode},
    solverTable[solver, {solverArgs}, 
    Table[{Sequence@@({##2}[[;;,1]]), Re[mode], Im[mode]}, {mode, #1}]&, 
    Flatten[#1,numTabArgs]&, 
    tableArgs]
]

Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["BulkSolverUtils`*"], Head[#] === Symbol &]];

End[];
EndPackage[];
