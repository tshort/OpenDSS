unit GeneratorVars;

{
  Definition of Generator Public Data Record for passing to DLLs and other object
}

interface

TYPE
    pTGeneratorVars = ^TGeneratorVars;

   {Generator state record}
   TGeneratorVars = packed Record

        Theta,      {Direct-Axis voltage magnitude & angle}
        Pshaft,
        Speed,
        w0,         {present Shaft Power and relative Speed, rad/sec, difference from Synchronous speed, w0}
                    {actual speed = Speed + w0}
        Hmass,      {Per unit mass constant}
        Mmass,      {Mass constant actual values (Joule-sec/rad}
        D, Dpu,     {Actual and per unit damping factors}
        kVArating,
        kVGeneratorBase,
        Xd, Xdp, Xdpp,   {machine Reactances, ohms}
        puXd, puXdp, puXdpp,   {machine Reactances, per unit}
        dTheta,
        dSpeed,   {Derivatives of Theta and Speed}
        ThetaHistory, SpeedHistory,   {history variables for integration}
        Pnominalperphase, Qnominalperphase  {Target P and Q for power flow solution, watts, vars}

               : Double;    { All Doubles }

        {32-bit integers}
        NumPhases,       {Number of phases}
        NumConductors,   {Total Number of conductors (wye-connected will have 4)}
        Conn           :Integer;   // 0 = wye; 1 = Delta

   End;

implementation

end.
