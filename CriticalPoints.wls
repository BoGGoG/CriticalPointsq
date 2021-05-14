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

qnms = CalcQNMs[{sols, maxOrder}, 0,0];

qnmsReIm = Map[{Re[#],Im[#]}&, qnms];
ShowPlot@ListPlot[qnmsReIm]





(*---- Playground ----*)


