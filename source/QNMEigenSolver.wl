(* ::Package:: *)

BeginPackage["QNMEigenSolver`"];

Unprotect["QNMEigenSolver`*"];
ClearAll["QNMEigenSolver`*", "QNMEigenSolver`Private`*"];

(* Begin Documentation/Declaration - I recommend to edit documentation via the GUI if possible.*)

eommatform::usage="eommatform[{regeom_1,regeom_2,...}, gridOrder, {field_1,field_2,...}, radialCoordinate, gridDiscretizeFunction, derivativeDiscretizeFunction] discretizes a list of regular equations of motion, regularEOMs, at gridOrder. The fields and radial coordinates must be supplied. You have to supply your own grid dicretization scheme. The gridDiscretizeFunction[gridOrder_PositiveInteger] -> (list of numbers of gridOrder+1).The derivativeDiscretizeFunction[gridOrder_PositiveInteger, derivativeOrder_NonNegative] -> ((gridOrder+1) by (gridOrder+1) derivative matrix).";
eommatform::usage::Japanese="eommatform[regularEOMs, gridOrder, {field_1,field_2,...}, radialCoordinate, gridDiscretizeFunction, derivativeDiscretizeFunction] discretizes a list of regular equations of motion, regularEOMs, at gridOrder. 
The fields and radial coordinates must be supplied. You have to supply your own grid dicretization scheme. 
The gridDiscretizeFunction[gridOrder_PositiveInteger] -> (list of numbers of gridOrder+1).
The derivativeDiscretizeFunction[gridOrder_PositiveInteger, derivativeOrder_NonNegative] -> ((gridOrder+1) by (gridOrder+1) derivative matrix).";

alphabetamat::usage="alphabetamat[{regeom_1,regeom_2,...}, gridOrder, {field_1,field_2,...}, radialCoordinate, frequency, gridDiscretizeFunction, derivativeDiscretizeFunction] convertes a list of regular equations of motion to a generlized eigenvalue problem. It return a 2 length list of two matrices, {A,B} that represents the generalized eigenvalue problem Av=wBv for where w (the frequency) is the eigenvalue and v is the eigenvector. You have to supply your own grid dicretization scheme. The gridDiscretizeFunction[gridOrder_PositiveInteger] -> (list of numbers of gridOrder+1).The derivativeDiscretizeFunction[gridOrder_PositiveInteger, derivativeOrder_NonNegative] -> ((gridOrder+1) by (gridOrder+1) derivative matrix).";
alphabetamat::usage::Japanese="alphabetamat[regularEOMs, gridOrder, {field_1,field_2,...}, radialCoordinate, frequency, gridDiscretizeFunction, derivativeDiscretizeFunction] convertes a list of regular equations of motion to a generlized eigenvalue problem. It return a 2 length list of two matrices, {A,B} that represents the generalized eigenvalue problem Av=wBv for where w (the frequency) is the eigenvalue and v is the eigenvector. 
You have to supply your own grid dicretization scheme. 
The gridDiscretizeFunction[gridOrder_PositiveInteger] -> (list of numbers of gridOrder+1).
The derivativeDiscretizeFunction[gridOrder_PositiveInteger, derivativeOrder_NonNegative] -> ((gridOrder+1) by (gridOrder+1) derivative matrix).";

convergentfilter::usage="convergentfilter[modesSet1,modesSet2,\"convergenceTolerance\"->10^-6] returns the modes in modesSet1 that are within within \"convergenceTolerance\"(default 10^-6) of to some other mode in modesSet2.";
convergentfilter::usage::Japanese="convergentfilter[modesSet1,modesSet2,\"convergenceTolerance\"->10^-6] returns the modes in modesSet1 that are within within \"convergenceTolerance\"(default 10^-6) of to some other mode in modesSet2.";

modespseudo::usage="modespseudo[{A,B}] returns all the eigenvalues from the generlized eigenvalue problem represented by {A,B}.
modespseudo[{{A_1,B_1},{A_2,B_2},{A_3,B_3}...}] returns the eigenvalues from the generlized eigenvalue problems represented by {{A_1,B_1},{A_2,B_2},{A_3,B_3}...}. The convergent modes are kept according to \"convergenceTolerance\"(default 10^-6).";
modespseudo::usage::Japanese="modespseudo[{A,B}] returns all the eigenvalues from the generlized eigenvalue problem represented by {A,B}.
modespseudo[{{A_1,B_1},{A_2,B_2},{A_3,B_3}...}] returns the eigenvalues from the generlized eigenvalue problems represented by {{A_1,B_1},{A_2,B_2},{A_3,B_3}...}. The convergent modes are kept according to \"convergenceTolerance\"(default 10^-6).";

eomToRootFunction::usage="eomToRootFunction[{regeom_1,regeom_2,...}, gridOrder, {field_1, field_2, ...}, radialCoord, frequencySymbol, momentumSymbol, gridFunc, derivativeFund]= Constructs a root functions for Regular EOMs given. This root function, F(w,k), is formated such that if w is a qnm than F=0. w is the frequency and k is the momentum.";
eomToRootFunction::usage::Japanese="eomToRootFunction[{regeom_1,regeom_2,...}, gridOrder, {field_1, field_2, ...}, radialCoord, frequencySymbol, momentumSymbol, gridFunc, derivativeFund]= Constructs a root functions for Regular EOMs given. This root function, F(w,k), is formated such that if w is a qnm than F=0. w is the frequency and k is the momentum.";

findQNMsParamSeedK::usage="findQNMsParamSeedK[seedMode,QNMRootFunc,kFunction,{t_0,t_f,dt},secantMethodFunc] this finds QNMs starting with with guesses parameterized by kFunction where t is the parameter. kFunction sets k, the momentum.
The secantMethodFunc must be provided for where it uses secant method. An example of this is in the RootFidning...wl file.";
findQNMsParamSeedK::usage::Japanese="findQNMsParamSeedK[seedMode,QNMRootFunc,kFunction,{t_0,t_f,dt},secantMethodFunc] this finds QNMs starting with with guesses parameterized by kFunction where t is the parameter. kFunction sets k, the momentum.
The secantMethodFunc must be provided for where it uses secant method. An example of this is in the RootFidning...wl file.";

Begin["`Private`"];
(*Options for Functions*)
$OPTIONS = {WorkingPrecision->MachinePrecision, "bounds"-> {0,1},"boundaryvanish"->True,"horizonvanish"->False, "convergenceTolerance"->10^-6, Tolerance->10^-10};

(*Help Functions*)
coefficientToMatrix[coef_,radialCoord_Symbol,grid_List] := DiagonalMatrix@Table[(coef/.{radialCoord -> gridpoint}),{gridpoint,grid}]

findLargestDerivativeOrder[regEqs_List, fields:{fieldP__Symbol}, radialCoord_Symbol] := Module[{foundDerOrders, minOrder = 0},
	foundDerOrders = Cases[regEqs, Derivative[order_][Alternatives[fieldP]][radialCoord]:>order, Infinity];
	Max@@(minOrder~Join~foundDerOrders)
]

findLargestFrequencyOrder[regEqs_List, frequency_Symbol] := Module[{foundfreqOrders, minOrder = 0,firstOrder},
	foundfreqOrders = Cases[Expand@Tr@regEqs, frequency^(order_):>order, Infinity];
	firstOrder = If[(D[Expand@Tr@regEqs,frequency]/.frequency->0) =!= 0,{1},{0}];
	Max@@(minOrder~Join~foundfreqOrders~Join~firstOrder)
]

derOrder[fieldsAndDer_] := fieldsAndDer/.{Derivative[order_][_][_]:>order,_->0};

Options[derBlockMatrix]=$OPTIONS;
derBlockMatrix[gridOrder_,derOrder_,derFunc_,fields_,opts:OptionsPattern[]] := Module[{numberOfFields,derMat},
	numberOfFields = Length@fields;
	derMat = derFunc[gridOrder,derOrder, WorkingPrecision-> OptionValue[WorkingPrecision], "bounds"-> OptionValue["bounds"]];
	ArrayFlatten@Table[If[row==col,1,0]*derMat,{row,numberOfFields},{col,numberOfFields}]
] 

(*eommatform Definition*)

Options[eommatform]= $OPTIONS;
SyntaxInformation[eommatform]={"ArgumentsPattern"->{_,_,{__},_,OptionsPattern[]}};

eommatform[regEqs_List, gridOrder_?((Positive[#]\[And]IntegerQ[#])&), fields:{fieldP__Symbol}, radialCoord_Symbol, gridFunc_, derFunc_, opts:OptionsPattern[]]:=
Module[
{grid, coefsAndDers,fieldsAndDers,maxDerOrder,coefDerToMat,eomMatrix},

	grid=gridFunc[gridOrder, WorkingPrecision-> OptionValue[WorkingPrecision], "bounds"-> OptionValue["bounds"]];

	maxDerOrder = findLargestDerivativeOrder[regEqs, fields, radialCoord];

	(*fieldsAndDers = Flatten@Table[Derivative[derOrder][field][radialCoord],{field,fields},{derOrder,0, maxDerOrder}];*)

	coefsAndDers = Table[{D[regEq, Derivative[derOrder][field][radialCoord]], derOrder}, {regEq,regEqs}, {field,fields}, {derOrder,0,maxDerOrder}];
	
	coefDerToMat := Function[{coef,derOrder}, 
						Dot[
							coefficientToMatrix[coef,radialCoord,grid], 
							derFunc[gridOrder,derOrder, WorkingPrecision-> OptionValue[WorkingPrecision], "bounds"-> OptionValue["bounds"]]
							]
					];
					
	eomMatrix = Apply[coefDerToMat,coefsAndDers,{3}];
	eomMatrix = ArrayFlatten@Sum[eomMatrix[[;;,;;,derOrder,;;,;;]],{derOrder,maxDerOrder+1}]

]

(*alphabetamat Definition*)

Options[alphabetamat]=$OPTIONS;
SyntaxInformation[alphabetamat]={"ArgumentsPattern"->{_,_,{__},_,_,OptionsPattern[]}};

alphabetamat[regEqs_List, gridOrder_?((Positive[#]\[And]IntegerQ[#])&), fields:{fieldP__Symbol}, radialCoord_Symbol, frequency_Symbol, gridFunc_, derFunc_, opts:OptionsPattern[]]:=
Module[
{eomMatrix, maxFreqOrder,zeroMat, firstDerMat, bMat, aMat},

eomMatrix = eommatform[regEqs, gridOrder, fields, radialCoord, gridFunc, derFunc, opts];

maxFreqOrder=findLargestFrequencyOrder[regEqs, frequency];

firstDerMat = derBlockMatrix[gridOrder,0,derFunc,fields, WorkingPrecision-> OptionValue[WorkingPrecision], "bounds"-> OptionValue["bounds"]];
zeroMat = firstDerMat*0;

aMat := Function[{row,col},
			Which[
				row==col && col==1,
				-eomMatrix,
				
				row==col,
				firstDerMat,
				
				True,
				zeroMat
				
			]/.{frequency->0}
		];

bMat := Function[{row,col},
			Which[
				row==1,
				D[eomMatrix,{frequency,col}]/Factorial[col],
				
				row==col+1,
				firstDerMat,
				
				True,
				zeroMat
				
			]/.{frequency->0}
		]; 

Switch[
	maxFreqOrder,
	
	0,
	Print["No "<>ToString[frequency]<>" found in the EOMs."];
	{-eomMatrix,eomMatrix*0}/.{frequency->0},
	
	_,
	Function[mat,ArrayFlatten@Array[mat,{maxFreqOrder,maxFreqOrder}]]/@{aMat,bMat}
]

]

(*modespseudo Definition*)

Options[modespseudo]=$OPTIONS;
SyntaxInformation[modespseudo]={"ArgumentsPattern"->{_,OptionsPattern[]},
								"ArgumentsPattern"->{{__},OptionsPattern[]}};

aBMatQ = (If[Head[#]===List && Length[#]==2 && (MatrixQ@*First)[#] && (MatrixQ@*Last)[#] && (And@@(NumericQ/@Flatten[#])),True,False])&

modespseudo[abMat_?aBMatQ, opts:OptionsPattern[]]:=Eigenvalues[abMat]

modespseudo[abMatList:{abMats__?aBMatQ}, opts:OptionsPattern[]]:=
Module[{modesList,filterFunc},

modesList = Table[modespseudo[abMat],{abMat, abMatList}];

filterFunc=convergentfilter[#1,#2,opts]&;

Fold[filterFunc, modesList]
]

(*convergentfilter Definition*)

Options[convergentfilter]=$OPTIONS;
SyntaxInformation[convergentfilter]={"ArgumentsPattern"->{_,_,OptionsPattern[]}};

convergentfilter[modesSet1_,modesSet2_,opts:OptionsPattern[]]:=
Module[{relativeError,tol=OptionValue["convergenceTolerance"]},

	relativeError = Function[{qnm1,qnm2},2*Abs[qnm1-qnm2]/(Abs[qnm1]+Abs[qnm2]+
							If[Abs[qnm1]+Abs[qnm2]==0,10^-14,0]),Listable];

	Table[
			If[ Min@@(relativeError[qnm,modesSet2])<tol,
				qnm,
				Nothing
				],
	{qnm,modesSet1}]
]

Options[eomToRootFunction]=$OPTIONS;
eomToRootFunction[eoms:{__}, gridOrder_, fields:{__}, radialCoord_, freq_,momentum_ ,gridF_, derF_,opts:OptionsPattern[]]:=Module[
{rhs,eomMat, wp=OptionValue[WorkingPrecision]},

eomMat = eommatform[eoms, gridOrder, fields, radialCoord, gridF, derF,opts];
eomMat[[1]] =First@ derF[gridOrder,0,opts];
rhs=First@derF[gridOrder,0,opts];
Function[{x,y},Last@LinearSolve[eomMat/.{freq->x,momentum->y},rhs]]
];

Options[findQNMsParamSeedK]= $OPTIONS;
findQNMsParamSeedK[seedMode_,QNMF_,kF_,xP:{xS__},secantMethod_,opts:OptionsPattern[]]:=Module[{kNext,qnm,nextWGuess,kAtParam,yy},

kAtParam = {seedMode};

Do[
	nextWGuess= Last@Last[kAtParam];(*Can make this smarter*)
	kNext = kF[yy];
	qnm=secantMethod[(QNMF[#,kNext]&),nextWGuess,nextWGuess+(1+1)I 10^(-5),opts];
	AppendTo[kAtParam,{kNext,qnm}]
,{yy,xS}];

kAtParam
]

(*End of Package*)

Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["QNMEigenSolver`*"], Head[#] === Symbol &]];

End[];
EndPackage[];
