#!/usr/bin/env wolframscript
(* ::Package:: *)

If[$ScriptCommandLine==={},SetDirectory@NotebookDirectory[],SetDirectory@DirectoryName@ExpandFileName@First[$ScriptCommandLine];];


Get[FileNameJoin[{Directory[],"source","PseudoSpectralUtils.wl"}]]
Get[FileNameJoin[{Directory[],"source","QNMEigenSolver.wl"}]]
Get[FileNameJoin[{Directory[],"source","BulkSolverUtils.wl"}]]
Get[FileNameJoin[{Directory[],"eoms","schwarzschild-black-brane-eoms.wls"}]]
Get[FileNameJoin[{Directory[],"source","RootFindingSolvers.wl"}]]


usage[]:=Print["\n
usage: wolframscript -f findQNMsEigen.wls <tensorEom|vectorEom|scalarEom> [options]

options \"name=default\":
	wp=2; (*working precision inputted as units of MachinePresicion*)
	gridLower=50;
	gridHigher=52;
	tol=10^(-10); (*tolerance*)
	kabsMax=5;
	kargDiv=5;
	absKsqr=Subdivide[0,kabsMax,kabsMax*5];
	argKsqr=Subdivide[0,2\[Pi],kargDiv];

examples:
	1: finding scalar modes with working precision 1(*MachinePresicion) and lower gridpoints
	
	wolframscript -f findQNMsEigen.wls scalarEom \"wp=1\" \"gridLower=25\" \"gridHigher=30\"

	2: finding tensor modes will all options set.
	
	wolframscript -f findQNMsEigen.wls tensorEom \"wp=2\" \"gridLower=48\" \"gridHigher=45\" \"tol=10^(-8)\" \"kabsMax=10\" \"absKsqr=Subdivide[0,kabsMax,kabsMax*2]\" \"argKsqr=Subdivide[0,2\[Pi],100]\"

\n
"]

wp=2; (*wp inputted as units of MachinePresicion*)
gridLower=50;
gridHigher=52;
tol=10^(-5);
kabsMax=5;
kargDiv=100;
absKsqr=Subdivide[0,kabsMax,kabsMax*5];
argKsqr=Subdivide[0,2\[Pi],kargDiv];

argv = Rest @ $ScriptCommandLine;
argc = Length @ argv;

Check[If[argc>1,Map[ToExpression,argv[[2;;]]]];,
Print["There was an error reading the options given. Quitting..."]; usage[]; Quit[]];

wp=wp*MachinePrecision;(*wp inputed as units of MachinePresicion*)

Check[eom = ToExpression[argv[[1]]], 
Print["Could name read out a symbol for the eom. Quitting..."]; usage[]; Quit[]];

eomName=ToString[argv[[1]]];
If[Head[eom]==Symbol,Print["There is no eom with the name: "<>eomName<>". Quitting..."]; usage[]; Quit[]];
eom=eom/.{k->Sqrt[ksqrA*Exp[I*ksqrTh]]};(*Using k^2 as the variable instead of k.*)

setTime=((Set[time,First[#]];Last[#])&)

abMats=N[funcTable[alphabetamat,
{{eom},gp,{F},u,w,gridps,derivativemat,WorkingPrecision->wp},{gp,gridLower,gridHigher}
],wp]//AbsoluteTiming//setTime
Print["Found the 'alpha' and 'beta' matrices for gridpoints ",gridLower," to ",gridHigher," in ",time," seconds."];

If[wp===MachinePrecision,
invABMats=abMats,
invABMats=((Quiet@Check[Inverse[#2] . #1,{#1,#2}]&)@@@abMats)//AbsoluteTiming//setTime;
Print["Found inverse of beta for some of the abMats in ", time, " seconds."]];


(*begin logging def*)
listTimes={};
totalTrials=Length[absKsqr]*Length[argKsqr]; 
completedTrails=0;

logPrintTime[input_]:=Module[{timediffs,aveTime,timeLeft,timeLeftStr},
AppendTo[listTimes,Now];
completedTrails+=1;

If[Length[listTimes]>1,
timediffs=Abs@*Subtract@@@Partition[listTimes,2,1];
aveTime = Mean@timediffs;
timeLeft=(totalTrials-completedTrails)*aveTime;

If[timeLeft>Quantity[1,"Minutes"],timeLeft=UnitConvert[timeLeft,"Minutes"]];
If[timeLeft>Quantity[1,"Hours"],timeLeft=UnitConvert[timeLeft,"Hours"]];
If[timeLeft>Quantity[1,"Days"],timeLeft=UnitConvert[timeLeft,"Days"]];

Print["(",completedTrails,"/",totalTrials,")"," completed \n Average time: ",ToString@aveTime,"\n Estimated total time left: ",ToString@timeLeft]
];

input]
(*end logging def*)


modes = funcTable[logPrintTime@*({N[ksqrA Exp[I ksqrTh],wp],modespseudoList[#,"convergenceTolerance"->tol]}&), {invABMats}, {ksqrA,absKsqr}, {ksqrTh,argKsqr}]//AbsoluteTiming//setTime;
modes = Apply[DeleteDuplicatesBy[Round[#,1/100]]@Table[{#1,mode},{mode,#2}]&,modes,{2}];

(*modes//First//First//Last//Print;*)
Print["Found all the modes in ", time, " seconds."];

If[
	Not@DirectoryQ[FileNameJoin[{Directory[],"data"}]],
	CreateDirectory[DirectoryQ[FileNameJoin[{Directory[],"data"}]]]
	]

fileWp=Round@N[wp/MachinePrecision]//ToString; (*wp inputted as units of MachinePresicion*)
filegridLower=gridLower//Round//ToString;
filegridHigher=gridHigher//Round//ToString;
filetol=-Log10[tol]//Round//ToString;
filekAbsMax=kabsMax//Round//ToString;
fileAbsDiv=(Length@absKsqr-1)//ToString;
fileArgsDiv=(Length@argKsqr-1)//ToString;

fileName=eomName;
fileName=fileName<>"-"<>fileWp<>"wp";
fileName=fileName<>"-"<>filegridLower<>"lower";
fileName=fileName<>"-"<>filegridHigher<>"higher";
fileName=fileName<>"-"<>filekAbsMax<>"absmax";
fileName=fileName<>"-"<>fileAbsDiv<>"absDiv";
fileName=fileName<>"-"<>fileArgsDiv<>"argDiv";

Put[modes,FileNameJoin[{Directory[],"data",fileName}]];
Print["Exported out "<>ToString@FileNameJoin[{Directory[],"data",fileName}]];
