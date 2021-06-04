(* ::Package:: *)

BeginPackage["RootFindingSolvers`"];

Unprotect["RootFindingSolvers`*"];
ClearAll["RootFindingSolvers`*", "RootFindingSolvers`Private`*"];

(* Begin Documentation/Declaration - I recommend to edit documentation via the GUI if possible.*)

secantRootFind::usage="secantRootFind[f, {{x_1,x_2,...}, {y_1, y_2,...}}] finds the roots f (f(0) = 0 using the Secant method."

Begin["`Private`"];
(*Options for Functions*)
rootFindingOptions = { WorkingPrecision->MachinePrecision, Tolerance->10^(-10),
                     NormFunction-> Norm}

Options[secantRootFind] = rootFindingOptions;
secantRootFind[func_, guess1_?NumericQ, guess2_?NumericQ, opts:OptionsPattern[]]:=
Module[{nextGuessFunc, nextGuess, currentGuesses, workingGuesses, steps=0,
        tol=OptionValue["Tolerance"], wp=OptionValue["WorkingPrecision"],
        res, reses={}},
    currentGuesses=workingGuesses=SetPrecision[{guess1,guess2},wp];

    (*Uses the secant recurrence relation*)
    nextGuessFunc = Function[{xm2,xm1}, (xm2*(func@xm1) - xm1*(func@xm2))/(func@xm1-func@xm2)];
    
    res=100;
    While[res>tol && steps<100, 
        nextGuess = nextGuessFunc@@workingGuesses;
        print(nextGuess);
        workingGuesses[[1]] = workingGuesses[[2]];
        workingGuesses[[2]] = nextGuess;
        AppendTo[currentGuesses, nextGuess];
        
        res=Norm@Apply[Subtract]@workingGuesses;
        AppendTo[reses,res];

        steps++;
    ];

    SetPrecision[Last[workingGuesses],-1-Log10[tol]]
]

(*End of Package*)

Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["RootFindingSolvers`*"], Head[#] === Symbol &]];

End[];
EndPackage[];
