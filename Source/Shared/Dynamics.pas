unit Dynamics;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Definitions of constants and structures for user-written dynamic generator models}

interface

CONST
      NumSolutionModes = 16;

    {Solution modes}
      SNAPSHOT = 0;
      DAILYMODE = 1;
      YEARLYMODE = 2;  // 8760 hour
      MONTECARLO1 = 3;
      LOADDURATION1 = 4;
      PEAKDAY = 5;
      DUTYCYCLE = 6;
      DIRECT = 7;
      MONTEFAULT = 8;  // Monte Carlo Fault Study
      FAULTSTUDY = 9;  // Run through all buses and compute Voc and Zsc; Then ask for fault current.
      MONTECARLO2 = 10;
      MONTECARLO3 = 11;
      LOADDURATION2 = 12;
      AUTOADDFLAG = 13;
      DYNAMICMODE = 14;
      HARMONICMODE = 15;
      GENERALTIME = 16;

TYPE


   {Variables needed for dynamics and user-written models.}
   TDynamicsRec = Packed Record
         {time vars}
       h,     // Time step size in sec for dynamics
       t,     // sec from top of hour
       tstart,
       tstop:Double;
       IterationFlag:Integer;  {0=New Time Step; 1= Same Time Step as last iteration}
       SolutionMode :Integer;   //  PEAKSNAP, DAILYMODE, YEARLYMODE, MONTECARLO, etc.  (see DSSGlobals)

   End;

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

               :Double;    { All Doubles }

        {32-bit integers}
        NumPhases,       {Number of phases}
        NumConductors,   {Total Number of conductors (wye-connected will have 4)}
        Conn           :Integer;   // 0 = wye; 1 = Delta

   End;

  


implementation


end.
