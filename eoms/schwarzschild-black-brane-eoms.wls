#!/usr/bin/env wolframscript
(* ::Package:: *)

(*These are EOMs for the Schwarzschild black brane*)
tensorEom = (-4 k^2 (1+u)+w (-2 I (1+u)+(4+u (3+u)) w)) F[u]+4 I (1+u) (I+u (w+u (I+w))) Derivative[1][F][u]-4 (-1+u) u (1+u)^2 Derivative[2][F][u];
scalarEom = (-4 k^2 (1+u) (4 u^3+k^2 (-3+u^2))-k^2 (21+u (18+u (4+3 u (2+u)))) w^2+3 (3+u (2+u)) w^4) F[u]-4 (1+u) ((k^2 (-3+2 u^2-3 u^4)+3 (1+u^2) w^2) Derivative[1][F][u]+u (-1+u^2) (k^2 (-3+u^2)+3 w^2) Derivative[2][F][u]);
vectorEom = (-4 k^4 (-1+u) (1+u)^2+k^2 (1+u) w (-2 I (1+u)^2 (-1+2 u)+(-8+u (1+u)^2) w)+w^3 (-2 I (1+u)+(4+u (3+u)) w)) F[u]+4 (1+u) ((k^2 (-1+u) (1+u)^2 (-1+u+I u w)+w^2 (-1+I u (w+u (I+w)))) Derivative[1][F][u]-u (-1+u^2) (k^2 (-1+u^2)+w^2) Derivative[2][F][u]);
