(* ::Package:: *)

BeginPackage["PseudoSpectralUtils`"];

Unprotect["PseudoSpectralUtils`*"];
ClearAll["PseudoSpectralUtils`*", "PseudoSpectralUtils`Private`*"];

(* Begin Documentation/Declaration - I recommend to edit documentation via the GUI if possible.*)

gridps::usage="gridps[order] gives order+1 Chebyshev gridpoint List (from 0 to 1 by default).";
gridps::usage::Japanese="gridps[order] \:3068\:306f\:30c1\:30a7\:30d3\:30b7\:30a7\:30d5\:306e\:3000order+1\:3000\:304f\:3089\:3044\:30b0\:30ea\:30c3\:30c9\:30dd\:30a4\:30f3\:30c8\:306e\:30b0\:30ea\:30c3\:30c9\:3092\:623b\:3055\:308c\:308b\:3002\:5143\:306b\:30b0\:30ea\:30c3\:30c9\:304c0\:304b\:30891\:307e\:3067\:3060\:3002";

derivativemat::usage="derivativemat[order,matrixpower] returns the \!\(\*SuperscriptBox[\(matrixpower\), \(th\)]\) order (matrixpower\[GreaterEqual]0) (order+1)x(order+1) matrix represented derivative as defined for Cardinal Functions. This code references Pg 570. of Chebyshev and Fourier Spectral Methods by John P. Boyd";
derivativemat::usage::Japanese="derivativemat[order,matrixpower]\:3068\:306f(order+1)x(order+1)\:306e\:884c\:5217\:304c\:8868\:793a\:306b\:8868\:3055\:308c\:308b\:7b2cmatrixpower\:306e\:30ab\:30ea\:30c0\:30ca\:30fc\:30eb\:306e\:5c0e\:95a2\:6570\:3067\:3042\:308a\:307e\:3059\:3002\:3000\:3053\:306e\:884c\:5217\:306fJohn P. Boyd\:306e\[OpenCurlyQuote]Chebyshev and Fourier Spectral Methods\[OpenCurlyQuote]\:306e570\:30da\:30fc\:30b8\:304b\:3089\:6765\:305f\:3067\:3042\:308a\:307e\:3059\:3002";

Begin["`Private`"];

(*gridps Definition*)

Options[gridps]={"bounds"-> {0,1},WorkingPrecision -> MachinePrecision};
SyntaxInformation[gridps]={"ArgumentsPattern"-> {_,OptionsPattern[]}};

gridps[gridOrder_?((Positive[#]\[And]IntegerQ[#])&),OptionsPattern[]]:=N[(Rescale[Cos[\[Pi]/gridOrder Range[0,gridOrder]],{-1,1},OptionValue["bounds"]]),OptionValue[WorkingPrecision]]

(*derivativemat Definition*)

Options[derivativemat]={"bounds"-> {0,1},WorkingPrecision -> MachinePrecision};
SyntaxInformation[derivativemat]={"ArgumentsPattern"-> {_,_,OptionsPattern[]}};

derivativemat[gridOrder_,0,OptionsPattern[]]:=N[IdentityMatrix[gridOrder+1],OptionValue[WorkingPrecision]]

derivativemat[gridOrder_,mp_?((Positive[#]\[And]IntegerQ[#])&),opts:OptionsPattern[]]:=N[
((2/(-Subtract@@OptionValue["bounds"]))^mp (Block[{derivativematrixGrid=gridps[gridOrder,"bounds"-> {-1,1},WorkingPrecision-> OptionValue[WorkingPrecision]],i,j},
MatrixPower[
Table[
Piecewise[
{{1/6 (1+2 gridOrder^2),i==0&&j==0},
{1/6 (-(1+2 gridOrder^2)),i==gridOrder&&j==gridOrder},
{-(derivativematrixGrid[[j+1]]/(2 (1-(derivativematrixGrid[[j+1]])^2))),i==j&&j<gridOrder&&j>0}},
((-1)^(i+j) (If[i==0||i==gridOrder,2,1]))/((If[j==0||j==gridOrder,2,1]) (derivativematrixGrid[[i+1]]-derivativematrixGrid[[j+1]]))],
{i,0,gridOrder},{j,0,gridOrder}],
mp]])),OptionValue[WorkingPrecision]]

(*Protecting all Package Functions*)

Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["PseudoSpectralUtils`*"], Head[#] === Symbol &]];

End[];
EndPackage[];
