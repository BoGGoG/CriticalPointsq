Print["CriticalPointsCalculator.wl loaded"];

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
