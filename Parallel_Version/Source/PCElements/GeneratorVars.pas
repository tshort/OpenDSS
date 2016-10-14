unit GeneratorVars;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  Definition of Generator Public Data Record for passing to DLLs and other object
}

interface

Uses  UComplex;

TYPE
    pTGeneratorVars = ^TGeneratorVars;

   {Generator public data/state variable structure}
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
        dSpeed,     {Derivatives of Theta and Speed}
        ThetaHistory,
        SpeedHistory,   {history variables for integration}
        Pnominalperphase,
        Qnominalperphase  {Target P and Q for power flow solution, watts, vars}
                          : Double;    { All Doubles }

        {32-bit integers}
        NumPhases,       {Number of phases}
        NumConductors,   {Total Number of conductors (wye-connected will have 4)}
        Conn           :Integer;   // 0 = wye; 1 = Delta

        { Revisons (additions) to structure ...
          Later additions are appended to end of the structure so that
          previously compiled DLLs do not break
          }

        VthevMag  : Double;    {Thevinen equivalent voltage for dynamic model}
        VThevHarm : Double;    {Thevinen equivalent voltage mag reference for Harmonic model}
        ThetaHarm : Double;    {Thevinen equivalent voltage angle reference for Harmonic model}
        VTarget   : Double;   // Target voltage for generator with voltage control
        Zthev     : Complex;
        XRdp      : Double;  // Assumed X/R for Xd'
   End;

implementation

end.
