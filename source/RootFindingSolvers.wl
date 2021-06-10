(* ::Package:: *)

BeginPackage["RootFindingSolvers`"];

Unprotect["RootFindingSolvers`*"];
ClearAll["RootFindingSolvers`*", "RootFindingSolvers`Private`*"];

(* Begin Documentation/Declaration - I recommend to edit documentation via the GUI if possible.*)

secantRootFind::usage="secantRootFind[f, {{x_1,x_2,...}, {y_1, y_2,...}}] finds the roots f (f(0) = 0 using the Secant method. Vector -> Vector problems the dimensions of the domain and image must be the same."

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
        workingGuesses[[1]] = workingGuesses[[2]];
        workingGuesses[[2]] = nextGuess;
        AppendTo[currentGuesses, nextGuess];
        
        res=Norm@Apply[Subtract]@workingGuesses;
        AppendTo[reses,res];

        steps++;
    ];

    SetPrecision[Last[workingGuesses],-1-Log10[tol]]
]

secantRootFind[func_, guess1:{__?NumericQ}, guess2:{__?NumericQ}, opts:OptionsPattern[]]:=
Module[{nextGuessFunc, nextGuess, currentGuesses, workingGuesses, steps=0,
        tol=OptionValue["Tolerance"], wp=OptionValue["WorkingPrecision"],
        res, reses={}, workingInvJacobian, nextInvJacFunc, inDers},

    currentGuesses=workingGuesses=SetPrecision[{guess1,guess2},wp];

    (*Find initial Jacobian*)
    (*workingInvJacobian = Module[{
        deltaX = Subtract@@workingGuesses, 
        deltaF = Subtract@@(func@@@workingGuesses)},

        Outer[Divide, deltaX, deltaF]
        ];*)
    workingInvJacobian=Module[
        {inDers=IdentityMatrix[Length[guess1]],f0,fd,partialX,h=10^(-10)},

            f0 = func@@guess1;
            Table[
                fd = func@@(guess1+h*inDer); (fd-f0)/h
            ,{inDer,inDers}]
        ];

    (*Define next step functions the Jacobian and for the guesses*)
    nextInvJacFunc[{xm2_, xm1_}, Jim1_] := Module[{
            deltaX = xm1-xm2, 
            deltaF = (func@@xm1) - (func@@xm2)},
            
            Jim1 + (1/Dot[deltaX,Jim1,deltaF])*
                    TensorProduct[deltaX - Dot[Jim1,deltaF], 
                                  Dot[deltaX,Jim1]]
        ];

    nextGuessFunc[xm1_, Jim1_] := xm1 - Dot[Jim1,func@@xm1];

    res=Infinity;
    While[res>tol && steps<100,
        workingInvJacobian = nextInvJacFunc[workingGuesses,workingInvJacobian];
        workingGuesses = {
                          Last[workingGuesses], (*the next x in now the old x*)
                          nextGuessFunc[ (*update the new guess*)
                            Last[workingGuesses], 
                            workingInvJacobian
                          ]
                         };

        nextGuess=Last[workingGuesses];

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
