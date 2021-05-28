(* ::Package:: *)

BeginPackage["QNMEigenSolver`"];

Unprotect["QNMEigenSolver`*"];
ClearAll["QNMEigenSolver`*", "QNMEigenSolver`Private`*"];

(* Begin Documentation/Declaration - I recommend to edit documentation via the GUI if possible.*)

eommatform::usage="eommatform[GridPointsMinusOne,RegularEquation] constructs a matrix representation linear homogeneous Equations of Motion (EOM) which is regular at it's boundries of r@1 (the Horizon) and r@0 (the Spacetime Boundary) for use in Mathematica's Linear Solve.";
eommatform::usage::Japanese="eommatform[Grid Points Minus One,RegularEquation]\:3068\:306f\:904b\:52d5\:65b9\:7a0b\:5f0f\:306e\:884c\:5217\:306e\:8868\:793a\:3092\:9020\:308b\:95a2\:6570\:3067\:3042\:308a\:307e\:3059\:3002\:3053\:306e\:884c\:5217\:306e\:8868\:793a\:304cMathematica\:306e\:884c\:5217\:65b9\:7a0b\:5f0f\:3092\:89e3\:304f";

alphabetamat::usage="alphabetamat[GridPointsMinusOne,RegularEquation] constructs a matrix representation linear homogeneous Equations of Motion (EOM) which is regular at it's boundries of r@1 (the Horizon) and r@0 (the Spacetime Boundary) for use in Mathematica's Eigenvalue function (in the generalized Eigen value problem \[Alpha] equal to \[Lambda]\[Beta] for matrices \[Alpha], \[Beta] and eigenvalue\[Lambda]). Couple EOMs this code works well in Eddington\[Dash]Finkelstein coordinates. The regularEquation variable can only depend of fields which only depend on one coordinate. The regularEquation's momentum parameter must be labeled as q. The number of field must equal the numner of equations too.";
alphabetamat::usage::Japanese="alphabetamat[Grid Points Minus One,RegularEquation]\:3068\:306f\:904b\:52d5\:65b9\:7a0b\:5f0f\:306e\:884c\:5217\:306e\:8868\:793a\:3092\:9020\:308b\:95a2\:6570\:3067\:3042\:308a\:307e\:3059\:3002\:3053\:306e\:884c\:5217\:306e\:8868\:793a\:304cMathematica\:306e\:56fa\:6709\:5024\:306e\:95a2\:6570\:306e\:30a4\:30f3\:30d7\:30c3\:30c8\:306e\:305f\:3081\:3067\:308a\:307e\:3059\:3002\:3053\:306e\:95a2\:6570\:4f7f\:3046\:306b\:306f\:5883\:70b9\:306e\:898f\:5247\:7684\:306a\:7279\:7570\:70b9\:304c\:5fc5\:8981\:3067\:3042\:308a\:307e\:3059\:3002\:5730\:5e73\:7dda\:306e\:70b9\:304c\:ff52\:ff1d\:ff11\:3068\:6642\:7a7a\:306e\:5883\:70b9\:304c\:ff52\:ff1d\:ff10\:3068\:30a8\:30c3\:30c7\:30a3\:30f3\:30c8\:30f3 - \:30d5\:30a3\:30f3\:30b1\:30eb\:30b7\:30e5\:30bf\:30a4\:30f3\:5ea7\:6a19\:304c\:4f7f\:308f\:308c\:3066\:3044\:308b\:3068\:5404\:3005\:306e\:5834\:306f\:4e00\:3064\:5024\:304c\:6301\:3063\:3066\:3044\:308b\:306e\:306f\:5fc5\:8981\:3067\:3042\:308a\:307e\:3059\:3002\:60f0\:6027\:306e\:540d\:524d\:3068\:306f\:ff51\:3068\:547c\:3070\:308c\:307e\:3059\:3002";

convergentfilter::usage="convergentfilter[listOfRawEigenValues,minimumconvergentorder] finds convergence of eigenvalues between with two sets of eigenvalues. Returns those eigenvalues which have a convergence less than \!\(\*SuperscriptBox[\(10\), \(-minimumconvergentorder\)]\)";
convergentfilter::usage::Japanese="convergentfilter[listOfRawEigenValues,minimumconvergentorder]\:3068\:306f\:5341\:5206\:306b\:53ce\:675f\:3057\:305f\:56fa\:6709\:5024\:3092\:63a2\:3057\:95a2\:6570\:3067\:3042\:308a\:307e\:3059\:3002 \!\(\*SuperscriptBox[\(10\), \(-minimumconvergentorder\)]\)\:4ee5\:5185\:306e\:56fa\:6709\:5024\:304c\:623b\:3089\:308c\:307e\:3059\:3002";

modespseudo::usage="modespseudo[RegularEquationsofMotion,{LowerGridpointBound,UpperGridpointBound}] returns eigenvalues that do not differ more than a given cutoff to a minimum of \!\(\*SuperscriptBox[\(10\), \(-mincutoff\)]\) between
the eigenvalues found with MHigh+1 gridpoint and MLow+1 gridpoints. This mincutoff is 1 by default. Eigenvectors may be outputted too if desired but the funtion will 
not outout them by default. Eigenvalues will be rounded to the  maximum decimal point which doesn't change \n modespseudo[{mat1,mat2},q]";
modespseudo::usage::Japanese="modespseudo[RegularEquationsofMotion,{LowerGridpointBound,UpperGridpointBound}]\:3068\:306f MHigh+1 \:3068 MLow+1 \:304f\:3089\:3044\:30b0\:30ea\:30c3\:30c9\:30dd\:30a4\:30f3\:30c8\:304c\:6bd4\:3079\:308b\:3068\:6027\:683c\:306a\:5c0f\:6570\:70b9\:307e\:3067\:306e\:56fa\:6709\:5024\:3092\:623b\:308a\:307e\:3059\:3002\:5143\:306bmincutoff\:304c\:ff11\:3060\:3051\:3069\:6700\:5c0f\:9650\:306a\:6bd4\:3079\:308b\:3068
\!\(\*SuperscriptBox[\(\:5c0f\:6570\:70b9\:306f10\), \(-mincutoff\)]\)\:3067\:3042\:308a\:307e\:3059\:3002\:3000\:56fa\:6709\:30d9\:30af\:30c8\:30eb\:304c\:6b32\:3057\:3051\:308c\:3070\:3053\:306e\:95a2\:6570\:306f\:56fa\:6709\:30d9\:30af\:30c8\:30eb\:3092\:89e3\:304f\:3053\:3068\:304c\:51fa\:6765\:307e\:3059\:3002\:3000\:3044\:3064\:3082\:56fa\:6709\:5024\:306f\:6700\:5927\:9650\:306e\:6027\:683c\:306e\:5c0f\:6570\:70b9\:306b\:5207\:308a\:6368\:3066\:3089\:308c\:307e\:3059\:3002";

Begin["`Private`"];
(*Options for Functions*)
$OPTIONS = {WorkingPrecision->MachinePrecision, "bounds"-> {0,1},"boundaryvanish"->True,"horizonvanish"->False, "convergenceTolerance"->10^-6};

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

(*End of Package*)

Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["QNMEigenSolver`*"], Head[#] === Symbol &]];

End[];
EndPackage[];
