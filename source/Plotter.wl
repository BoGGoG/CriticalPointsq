(*qnms: {{k1, m1, qnms1}, {k2, m2, qnms2}, ...}*)

Options[RewImwPlot] = Options[ListPlot]
RewImwPlot[qnms_, opts:OptionsPattern[]] := Block[{data},
    data = Table[Thread@{Re@qnms[[i,3]], Im@qnms[[i,3]]}, {i,1,Length@qnms}];
    ListPlot[
        data,
        AxesLabel -> {"Re\[Omega]", "Im\[Omega]"},
        opts
    ]
];

Options[RekRewPlot] = Options[ListPlot]
RekRewPlot[qnms_, opts:OptionsPattern[]] := Block[{data},
    data = Table[Thread@{Re@qnms[[i,1]], Re@qnms[[i,3]]}, {i,1,Length@qnms}];
    ListPlot[
        data,
        AxesLabel -> {"Re k", "Re w"},
        opts
    ]
];

Options[RekImwPlot] = Options[ListPlot]
RekImwPlot[qnms_, opts:OptionsPattern[]] := Block[{data},
    data = Table[Thread@{Re@qnms[[i,1]], Im@qnms[[i,3]]}, {i,1,Length@qnms}];
    ListPlot[
        data,
        AxesLabel -> {"Re k", "Re w"},
        opts
    ]
];
