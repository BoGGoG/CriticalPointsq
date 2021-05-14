(*qnms: {{k1, m1, qnms1}, {k2, m2, qnms2}, ...}*)

RewImwPlot[qnms_] := Block[{data},
    data = Table[Thread@{Re@qnms[[i,3]], Im@qnms[[i,3]]}, {i,1,Length@qnms}];
    ListPlot[
        data,
        AxesLabel -> {"Re\[Omega]", "Im\[Omega]"}
    ]
];

RekRewPlot[qnms_] := Block[{data},
    data = Table[Thread@{Re@qnms[[i,1]], Re@qnms[[i,3]]}, {i,1,Length@qnms}];
    ListPlot[
        data,
        AxesLabel -> {"Re k", "Re w"}
    ]
];

RekImwPlot[qnms_] := Block[{data},
    data = Table[Thread@{Re@qnms[[i,1]], Im@qnms[[i,3]]}, {i,1,Length@qnms}];
    ListPlot[
        data,
        AxesLabel -> {"Re k", "Re w"}
    ]
];
