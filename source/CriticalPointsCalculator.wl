Print["CriticalPointsCalculator.wl loaded"];

Setup::usage = "Setup[] sets the differential equation, indicial exponent and the expansion
for the scalar field. This function should be the first thing you run after loading this file.";

Setup[] := Block[{},
    phiEq = (-((1 + u^2)/(u - u^3)))*Derivative[1][phi][u] + (((m^2 + kk^2*u)*(-1 + u^2) + u*w^2)/(4*u^2*(-1 + u^2)^2))*phi[u] + 
        Derivative[2][phi][u]; 

    phiEqRS=Collect[FullSimplify[phiEq],{phi''[u],phi'[u],phi[u]}];

    indicialExp = -((I w)/4);
    phiExpansion[order_] := (1-#)^indicialExp Sum[c[i] (#-1)^i, {i,0,order}] &;

    Print["phiEq = ", phiEqRS];
    Print["indicialExp = ", indicialExp];
    Print["function phiExpansion[order][u] defined"];
];

SolvePerturbatively::usage = "SolvePerturbatively[order] solves phiEq set in Setup[] up to order 7.
The output (sols) can then be used as input for CalcQNMs."

SolvePerturbatively[maxOrder_:7] := Block[{eq, sol, sols, order},
    (* first solve without plugging lower order solutions in
    to not blow up equations so much *)
    sols = {};
    Do[
        eq = phiEqRS /. phi -> phiExpansion[order];
        eq = eq / (1-u)^indicialExp;
        eq = Normal@Series[eq, {u,1,order}]; (* does this introduce errors? *)
        eq = Coefficient[eq/.u->x+1, x, order-2];
        sol = Solve[eq == 0, c[order]] // Simplify;
        AppendTo[sols, sol[[1,1]]];
    , {order,1,maxOrder}];

    (* plug solutions of order i<order into sol[order] *)
    values = Values@sols;

    Do[
        sols[[j]] = sols[[j]] /. sols[[1;;(j-1)]]; // Simplify
        , {j,1,Length@sols}];

    sols
];

CalcQNMs::usage = "CalcQNMs[{sols, order}, k, m, c0:1] calculates the QNMs by solving the spectral curve. 
order needs to be the same as in sols.
Output: {k, m, qnms}

Can also be called as CalcQNMs[{sols, order}, kList, mList, c0:1] to calculate the qnms for every combination of k and m in the two lists. This version als has a parallel option 'Parallel' -> True (default: False).
Output: {{k1, m1, qnms1}, {k2, m2, qnms2}, ...}
"
CalcQNMs[{sols_, maxOrder_}, k_, mm_, c0_:1] := Block[{phiSol, spectralCurve, qnms},
    phiSol = phiExpansion[maxOrder][u] /. sols;
    spectralCurve[ww_, q_, mmm_] := phiSol / (-1+u)^(-((I w)/4)) /. {kk->q, w->ww, m->mmm, u->0, c[0] -> c0} // Simplify; 
    qnms = w/.Solve[spectralCurve[w,0,0]==0,w];
    {k, mm, qnms}
];

Options[CalcQNMs] = {"Parallel" -> False};
CalcQNMs[{sols_, maxOrder_}, k_?ListQ, mm_?ListQ, c0_:1, OptionsPattern[]] := Block[{},
    If[OptionValue["Parallel"],
        CalcQNMsParallel[{sols, maxOrder}, k, mm, c0]
        , (*else*)
        CalcQNMsNotParallel[{sols, maxOrder}, k, mm, c0]
    ]
];

CalcQNMsNotParallel[{sols_, maxOrder_}, k_?ListQ, mm_?ListQ, c0_:1] := Block[{phiSol, spectralCurve, qnms, kmTuples, km},
    phiSol = phiExpansion[maxOrder][u] /. sols;
    spectralCurve[ww_, q_, mmm_] := phiSol / (-1+u)^(-((I w)/4)) /. {kk->q, w->ww, m->mmm, u->0, c[0] -> c0} // Simplify; 

    kmTuples = Tuples[{k, mm}];

    Table[
        {
            km[[1]], 
            km[[2]], 
            w/.Solve[spectralCurve[w,km[[1]],km[[2]]]==0,w]
        }, {km, kmTuples}
    ]
];

CalcQNMsParallel[{sols_, maxOrder_}, k_?ListQ, mm_?ListQ, c0_:1] := Block[{phiSol, spectralCurve, qnms, kmTuples, km},
    phiSol = phiExpansion[maxOrder][u] /. sols;
    spectralCurve[ww_, q_, mmm_] := phiSol / (-1+u)^(-((I w)/4)) /. {kk->q, w->ww, m->mmm, u->0, c[0] -> c0} // Simplify;

    kmTuples = Tuples[{k, mm}];

    ParallelTable[
        {
            km[[1]],
            km[[2]],
            w/.Solve[spectralCurve[w,km[[1]],km[[2]]]==0,w]
        }, {km, kmTuples}
    ]
];


CToReIm::messages = "CToReIm[number]: complex number -> {real part, im part}";
CToReIm = ReIm;

QNMsCToReIm::messages = "QNMsCToReIm[qnms] takes qnms in the format {k, m, qnm} -> {{Re k, Im k}, {Re m, Im m}, {Re w, Im w}}"
QNMsCToReIm[qnms_] := Map[{CToReIm[#[[1]]], CToReIm[#[[2]]], Map[CToReIm,#[[3]]]} &, qnms];

ComplexCircle::messages = "ComplexCircle[radius, {startAngle, endAngle}, nPoints] returns points in a circle around center.";
ComplexCircle[center_, radius_, {startAngle_, endAngle_}, nPoints_] := Block[{},
    angles = Subdivide[startAngle, endAngle, nPoints+1][[;;-2]];
    Table[center + Exp[I angle], {angle, angles}]
];
