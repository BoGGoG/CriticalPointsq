#!/usr/bin/env wolframscript
(* ::Package:: *)

If[$ScriptCommandLine==={},SetDirectory@NotebookDirectory[],SetDirectory@DirectoryName@ExpandFileName@First[$ScriptCommandLine];];


Get[FileNameJoin[{Directory[],"source","PseudoSpectralUtils.wl"}]]
Get[FileNameJoin[{Directory[],"source","QNMEigenSolver.wl"}]]
Get[FileNameJoin[{Directory[],"source","BulkSolverUtils.wl"}]]
Get[FileNameJoin[{Directory[],"eoms","schwarzschild-black-brane-eoms.wls"}]]
Get[FileNameJoin[{Directory[],"source","RootFindingSolvers.wl"}]]


argv = Rest @ $ScriptCommandLine;
argc = Length @ argv;

Check[eom = ToExpression[argv[[1]]], 
Print["Could name read out a symbol for the eom. Quitting..."]; Quit[]]

eomName=ToString[argv[[1]]];

If[Head[eom]==Symbol,Print["There is no eom with the name: "<>eomName<>". Quitting..."]; Quit[]];

eom=eom/.{k->Sqrt[ksqr]};(*Using k^2 as the variable instead of k.*)

wp=4*MachinePrecision;
seedGridOrderLower=40;
seedGridOrderHigher=42;
trunkBranchGridOrder=80;
tol=10^(-5)
maxRootModes=4;
kabsMax=3;

trunkAbsKsqr=Subdivide[0,kabsMax,kabsMax*5];
trunkArgKsqr=Subdivide[0,4\[Pi],200];

Options[findQNMTree]=Options[findQNMsParamSeedK];
findQNMTree[rootModes_,rootFunc_,kAbsList_,kArgList_,secantRootFunc_,opts:OptionsPattern[]]:=
Module[{trunkModes,argFunc,trunkModeK,foundModes},

foundModes=Table[
trunkModes = findQNMsParamSeedK[rootMode,rootFunc,Identity,{Rest@kAbsList},secantRootFunc,opts];
Table[
    Check[
        trunkModeK=First[trunkMode];
        argFunc=Function[th,Evaluate[trunkModeK]*Exp[I th]];
        findQNMsParamSeedK[trunkMode,rootFunc,argFunc,{Rest@kArgList},secantRootFunc,opts]//EchoFunction[ToString[eomName]<>": found modes",N@*(Take[#,UpTo[3]]&)]
    ,Nothing]
,{trunkMode,trunkModes}]
,{rootMode,rootModes}];

foundModes=N[foundModes,OptionValue[WorkingPrecision]];

foundModes
]

seedModeAlphaBeta = funcTable[alphabetamat,
{{eom/.{ksqr->0}},gp,{F},u,w, gridps, derivativemat, WorkingPrecision->wp},
{gp,seedGridOrderLower,seedGridOrderHigher}
];

seedModes = (Take[#, UpTo[maxRootModes]] &)@DeleteDuplicatesBy[Round[#, 1/10] &]@SortBy[Abs@*Last]@Map[{0,#}&]@modespseudo[seedModeAlphaBeta];

Print["Found the seed modes of "<> eomName];

rootFunc=eomToRootFunction[{eom}, trunkBranchGridOrder,{F},u,w,ksqr,gridps, derivativemat,WorkingPrecision->wp];

treeModes=findQNMTree[seedModes,rootFunc,trunkAbsKsqr,trunkArgKsqr,secantRootFind,WorkingPrecision->wp, Tolerance->tol];

Put[treeModes,FileNameJoin[{Directory[],"data",eomName<>"-tree-modes"}]];
Print["Exported out "<>ToString[FileNameJoin[{Directory[],"data",eomName<>"-tree-modes"}]]];
