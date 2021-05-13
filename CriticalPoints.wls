#!/usr/bin/env wolframscript

pgrep = RunProcess[{"pgrep", "-a", "wolframscript"}];
scriptRelativePath = Last[StringSplit[ pgrep["StandardOutput"], " " ]];
scriptPath = ExpandFileName[ FileNameJoin[{Directory[], scriptRelativePath}] ];
scriptDirectory = DirectoryName[ scriptPath ];
scriptName = Last[FileNameSplit[ scriptPath ]];
sourceFolder = "source";
sourcePath = FileNameJoin[{scriptDirectory, sourceFolder}];
$Path=Once[Append[$Path,sourcePath]];
Get[FileNameJoin[{sourcePath, "VIMHelper.wl"}]];

Get[FileNameJoin[{sourcePath, "CriticalPointsCalculator.wl"}]];

Setup[]

maxOrder = 7;
sols = SolvePerturbatively[maxOrder];

phiSol = phiExpansion[maxOrder][u] /. sols;

spectralCurve[ww_, k_, mm_] := phiSol / (-1+u)^(-((I w)/4)) /. {kk->k, w->ww, m->mm, u->0};

qnms = w/.Solve[spectralCurve[w,0,0]==0,w] // N

qnmsReIm = Map[{Re[#],Im[#]}&, qnms];
ShowPlot@ListPlot[qnmsReIm]


(*---- Playground ----*)


