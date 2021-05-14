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

maxOrder = 8;
sols = SolvePerturbatively[maxOrder];

qnms = CalcQNMs[{sols, maxOrder}, 0.4,0];

kIn = {0, 1/2, 1, 3/2, 2};
wIn = {0};

qnms = CalcQNMs[{sols, maxOrder}, kIn, wIn, 
    "Parallel" -> True];

qnmsReIm = QNMsCToReIm[qnms] 

p1 = ListPlot[Table[qnmsReIm[[i,3]], {i,1,3}]];
ShowPlot@p1










(*---- Playground ----*)


