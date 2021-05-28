#!/usr/bin/env wolframscript
(* ::Package:: *)

(*These are EOMs for the Schwarzschild black brane*)
tensorEom = (-4 k^2 (1+u)+w (-2 I (1+u)+(4+u (3+u)) w)) F[u]+4 I (1+u) (I+u (w+u (I+w))) Derivative[1][F][u]-4 (-1+u) u (1+u)^2 Derivative[2][F][u];
scalarEom = (-4 k^4 (1 + u) (-3 + u^2) + 3 w^3 (-2 I (1 + u) + (4 + u (3 + u)) w) + k^2 (-16 u^3 (1 + u) - 2 I (1 + u) (-3 + u^2 (5 + 4 u)) w + (-24 + u (-21 + u + 3 u^2 + u^3)) w^2)) F[u] + 4 (1 + u) (I (k^2 (-I (3 - 2 u^2 + 3 u^4) + u (1 + u) (-3 + u^2) w) + 3 w^2 (I + u (w + u (I + w)))) Derivative[1][F][u] - u (-1 + u^2) (k^2 (-3 + u^2) + 3 w^2) Derivative[2][F][u]);
vectorEom = (-4 k^4 (-1+u) (1+u)^2+k^2 (1+u) w (-2 I (1+u)^2 (-1+2 u)+(-8+u (1+u)^2) w)+w^3 (-2 I (1+u)+(4+u (3+u)) w)) F[u]+4 (1+u) ((k^2 (-1+u) (1+u)^2 (-1+u+I u w)+w^2 (-1+I u (w+u (I+w)))) Derivative[1][F][u]-u (-1+u^2) (k^2 (-1+u^2)+w^2) Derivative[2][F][u]);
