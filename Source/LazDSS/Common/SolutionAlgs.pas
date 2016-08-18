unit SolutionAlgs;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{ Solution Algorithms}

{
   9-20-00  Added SolveDynamic

   1/22/01 Added SolutionAbort Check wherever solution in a potentially long loop
   4/2/04  Updated SolutionAbort to work through redirect files  and long scripts
}

interface

 FUNCTION SolveMonte1:Integer;   // Solve Monte Carlo Solution
 FUNCTION SolveMonte2:Integer;   // Solve Monte Carlo Solution
 FUNCTION SolveMonte3:Integer;   // Solve Monte Carlo Solution
 FUNCTION SolveMonteFault:Integer;  // Solve Monte Carlo Fault Study
 FUNCTION SolveFaultStudy:Integer;  // Full Fault Study
 FUNCTION SolveDaily:Integer;    // Solve Following Daily Cycle
 FUNCTION SolvePeakDay:Integer;   // Solve Following Daily Cycle at peak load
 FUNCTION SolveYearly:Integer;   // Solve Following Yearly Cycle
 FUNCTION SolveDuty:Integer;     // Solve Following Duty Cycle
 FUNCTION SolveDynamic:Integer;  // Solve Dynamics
 FUNCTION SolveLD1:Integer;      // solve Load-Duration Curve, 1
 FUNCTION SolveLD2:Integer;      // solve Load-Duration Curve, 2
 FUNCTION SolveHarmonic:Integer;
 FUNCTION SolveHarmonicT:Integer;  // Sequential-Time Harmonics, Added 07-06-2015
 FUNCTION SolveHarmTime:Integer;  // solve harmonics vs time (like general time mode) created by Davis Montenegro 25/06/2014
 FUNCTION SolveGeneralTime:Integer;

  PROCEDURE ComputeYsc(iB:integer);
  PROCEDURE ComputeAllYsc;
  Procedure IntegratePCStates;
  PROCEDURE EndOfTimeStepCleanup;
  PROCEDURE FinishTimeStep ;

implementation

Uses ArrayDef, DSSGlobals, CmdForms,  Utilities, SysUtils, MathUtil, Math, Fault, uComplex, YMatrix,
     PCElement, Spectrum, Vsource, Isource, KLUSolve;

VAR ProgressCount:Integer;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
PROCEDURE FinishTimeStep;
{
   Cample Cleanup and increment time

   For custom solutions.

}
Begin
    MonitorClass.SampleAll;
    EndOfTimeStepCleanup;
    ActiveCircuit.Solution.Increment_time;
End;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
PROCEDURE EndOfTimeStepCleanup;
{
   Put stuff in this procedure that needs to happen at the end of the time step
   in main solution loops (see below)
}
Begin
    StorageClass.UpdateAll;
    InvControlClass.UpdateAll;
    ExpControlClass.UpdateAll;

    // End of Time Step Timer
    ActiveCircuit.Solution.UpdateLoopTime;
    MonitorClass.SampleAllMode5;  // sample all mode 5 monitors to get timings
End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
PROCEDURE Show10PctProgress(i, N:Integer);

Begin
    If NoFormsAllowed Then Exit;

    If ((i*10) div N) > ProgressCount Then
    Begin
        Inc(ProgressCount);
        ShowPctProgress( ProgressCount * 10);
    End;
End;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION SolveYearly:Integer;

VAR
   N, Twopct :Integer;

Begin
 Result := 0;
 ProgressCaption( 'Solving Year '+ IntToStr(ActiveCircuit.Solution.Year));
 ProgressCount := 0;
 ShowPctProgress(ProgressCount);

 WITH ActiveCircuit, ActiveCircuit.Solution Do
 Begin
  Try
    IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage elements
    IF Not DIFilesAreOpen then EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired   Creates DI_Totals
    Twopct := Max(NumberOfTimes div 50, 1);
    FOR N := 1 TO NumberOfTimes Do
      IF Not SolutionAbort Then With Dynavars do Begin
          Increment_time;
          DefaultHourMult := DefaultYearlyShapeObj.getmult(dblHour);
          IF PriceCurveObj <> NIL THEN PriceSignal := PriceCurveObj.GetPrice(dblHour);
          SolveSnap;
          MonitorClass.SampleAll;  // Make all monitors take a sample
          EnergyMeterClass.SampleAll; // Make all Energy Meters take a sample

          EndOfTimeStepCleanup;

          If (N mod Twopct)=0 Then ShowPctProgress((N*100) div NumberofTimes);
      End;
  Finally
    ProgressHide;
    MonitorClass.SaveAll;
    // EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files    See DIFilesAreOpen Logic
  End;
 End;
End;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION SolveDaily:Integer;

{
  Solves following the daily load curve.
  Stepsize defaults to 1 hr and number of times = 24.
  Load is modified by yearly growth, time of day, and global load multiplier.
}

VAR
   N:Integer;

Begin
   Result := 0;

   WITH ActiveCircuit, ActiveCircuit.Solution Do
   Begin
      // t:=0.0;
      // MonitorClass.ResetAll;
      // EnergyMeterClass.ResetAll;
    Try

      IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters
      IF Not DIFilesAreOpen then EnergyMeterClass.OpenAllDIFiles;   // Append Demand Interval Files, if desired

      FOR N := 1 TO NumberOfTimes Do
        IF Not SolutionAbort Then With DynaVars Do Begin
            Increment_time;
            DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);
            IF PriceCurveObj<> NIL THEN PriceSignal := PriceCurveObj.GetPrice(dblHour);
            SolveSnap;
            MonitorClass.SampleAll;  // Make all monitors take a sample
            EnergyMeterClass.SampleAll; // Make all Energy Meters take a sample

            EndOfTimeStepCleanup;

        End;

    Finally
      MonitorClass.SaveAll;
      EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
    End; {Try}
   End;  {WITH}
End;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION SolvePeakDay:Integer;

{
 Solves peak day

    Takes the given load kW and assumes it represents the peak value.
    Load is modified by daily load curve and growth factor for the year.
    'h' defaults to 3600 (1 hr) but can be reset to anything.
    Differs from Daily mode in that the global load multiplier is ignored.
}

VAR
   N:Integer;

Begin
   Result := 0;
   WITH ActiveCircuit, ActiveCircuit.Solution Do
   Begin
        DynaVars.t:=0.0;

        // MonitorClass.ResetAll;
        // EnergyMeterClass.ResetAll;
     Try
        DynaVars.intHour := 0; DynaVars.dblHour := 0.0;
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
        If Not DIFilesAreOpen Then EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

        FOR N := 1 TO NumberOfTimes Do
        IF Not SolutionAbort Then With DynaVars Do Begin
            Increment_time;
            DefaultHourMult := DefaultDailyShapeObj.GetMult(dblHour);
            IF PriceCurveObj<> NIL THEN PriceSignal := PriceCurveObj.GetPrice(dblHour);
            SolveSnap;
            MonitorClass.SampleAll;  // Make all monitors take a sample
            EnergyMeterClass.SampleAll; // Make all Energy Meters take a sample

            EndOfTimeStepCleanup;

        End;
      Finally
        MonitorClass.SaveAll;
        EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
      End;
     End;  {WITH}
End;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION SolveDuty:Integer;

VAR
   N, TwoPct:Integer;

Begin
   Result := 0;

   ProgressCaption( 'Duty Cycle Solution');
   ProgressCount := 0;
   ShowPctProgress(0);

   WITH ActiveCircuit, ActiveCircuit.Solution Do
   Begin
     //   t:=0.0;
        // MonitorClass.ResetAll;
     TwoPct := Max(1, NumberOfTimes div 50);
      Try
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
        FOR N := 1 TO NumberOfTimes Do
        IF Not SolutionAbort Then With DynaVars Do Begin
            Increment_time;
            DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);
            // Assume pricesignal stays constant for dutycycle calcs
            SolveSnap;
            MonitorClass.SampleAll;  // Make all monitors take a sample

            EndOfTimeStepCleanup;


            If (N mod Twopct)=0 Then ShowPctProgress((N*100) div NumberofTimes);
        End;
      Finally
        MonitorClass.SaveAll;
        ProgressHide;
      End;
    End;
End;

FUNCTION SolveGeneralTime:Integer;

{
   For Rolling your own solution modes
}
VAR
   N:Integer;

Begin
   Result := 0;

   WITH ActiveCircuit, ActiveCircuit.Solution Do
   Begin
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
        FOR N := 1 TO NumberOfTimes Do
          IF Not SolutionAbort Then With DynaVars Do
          Begin
              {Compute basic multiplier from Default loadshape to use in generator dispatch, if any}
                DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);

                SolveSnap;

                FinishTimeStep;
  
          End;
    End;
End;




//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure IntegratePCStates;
 {Integrate states in all PC Elements.  At present, only PC Elements
  can have dynamic states}

Var
   pcelem: TPCElement;

Begin
    With ActiveCircuit Do Begin
        pcelem := PCelements.First;
        WHILE pcelem <> NIL Do Begin
              pcelem.IntegrateStates;
              pcelem := PCelements.Next;
        End;
    End;
End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION SolveDynamic:Integer;

VAR
   N:Integer;

Begin
   Result := 0;

   WITH ActiveCircuit, ActiveCircuit.Solution Do
   Begin
     Try
        SolutionInitialized := True; // If we're in dynamics mode, no need to re-initialize.
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
        FOR N := 1 TO NumberOfTimes Do
        IF Not SolutionAbort Then With DynaVars Do Begin
          Increment_time;
          DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);
          // Assume price signal stays constant for dynamic calcs
       {Predictor}
          IterationFlag := 0;
          IntegratePCStates;
          SolveSnap;
       {Corrector}
          IterationFlag := 1;
          IntegratePCStates;
          SolveSnap;
          MonitorClass.SampleAll;  // Make all monitors take a sample

          EndOfTimeStepCleanup;

        End;
      Finally
        MonitorClass.SaveAll;
      End;
    End;
End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION SolveMonte1:Integer;

VAR
  N:Integer;

Begin
   Result := 0;

   WITH ActiveCircuit, ActiveCircuit.Solution Do
   Begin
     Try
        LoadMultiplier := 1.0;   // Always set with prop in case matrix must be rebuilt
        IntervalHrs := 1.0;     // needed for energy meters and storage devices
        DynaVars.intHour := 0;  DynaVars.dblHour := 0.0;// Use hour to denote Case number
        DynaVars.t := 0.0;

        // MonitorClass.ResetAll;
        // EnergyMeterClass.ResetAll;

        ProgressCaption( 'Monte Carlo Mode 1, ' + IntToStr(NumberofTimes) + ' Random Loads.');
        ProgressCount := 0;

        FOR N := 1 TO NumberOfTimes Do
        If Not SolutionAbort Then  Begin
            Inc(DynaVars.intHour);
            SolveSnap;
            MonitorClass.SampleAll;  // Make all monitors take a sample
            EnergyMeterClass.SampleAll;  // Make all meters take a sample
            Show10PctProgress(N, NumberOfTimes);
        End
        Else  Begin
           ErrorNumber := SOLUTION_ABORT;
           CmdResult := ErrorNumber;
           GlobalResult := 'Solution Aborted';
           Break;
        End;
     Finally
        MonitorClass.SaveAll;
        ProgressHide;
     End;
   End;

End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION SolveMonte2:Integer;

// Do a daily load solution for several Random days

VAR
  i, N, Ndaily:Integer;

Begin
    Result := 0;

    WITH ActiveCircuit, ActiveCircuit.solution
    Do Begin
     Try
        DynaVars.t := 0.0;
        DynaVars.intHour := 0;   DynaVars.dblHour := 0.0;
        // MonitorClass.ResetAll;
        // EnergyMeterClass.ResetAll;
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
        Ndaily := Round(24.0 / IntervalHrs);

        If Not DIFilesAreOpen Then EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

        ProgressCaption('Monte Carlo Mode 2, ' + IntToStr(NumberofTimes) + ' Days.');
        ProgressCount := 0;

        FOR N := 1 TO NumberOfTimes Do

        If   NOT SolutionAbort
        THEN Begin       // Number of Days

          // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
          Case Randomtype of
           UNIFORM:  LoadMultiplier := Random;  // number between 0 and 1
           GAUSSIAN: LoadMultiplier := Gauss(DefaultDailyShapeObj.Mean, DefaultDailyShapeObj.StdDev );
          End;

          With DynaVars Do FOR i := 1 to Ndaily Do
          Begin
            Increment_time;
            DefaultHourMult := DefaultDailyShapeObj.GetMult(dblHour);
            SolveSnap;

            MonitorClass.SampleAll;  // Make all monitors take a sample
            EnergyMeterClass.SampleAll;  // Make all meters take a sample

            EndOfTimeStepCleanup;

          End;

          Show10PctProgress(N, NumberOfTimes);

        End
        ELSE Begin
           ErrorNumber := SOLUTION_ABORT;
           CmdResult := ErrorNumber;
           GlobalResult := 'Solution Aborted.';
           Break;
        End;
      Finally
        MonitorClass.SaveAll;
        EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
        ProgressHide;
      End;
    End;
End;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION SolveMonte3:Integer;

// Hold time fixed and just vary the global load multiplier

VAR
  N:Integer;

Begin
    Result := 0;

    WITH ActiveCircuit, ActiveCircuit.Solution
    Do Begin
    // Time must be set beFore entering this routine
      Try
        // MonitorClass.ResetAll;
        // EnergyMeterClass.ResetAll;
        IntervalHrs := 1.0;  // just get per unit energy and multiply result as necessary

        If Not DIFilesAreOpen Then EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

        ProgressCaption( 'Monte Carlo Mode 3, ' + IntToStr(NumberofTimes) + ' Different Load Levels.');
        ProgressCount := 0;

        DefaultHourMult := DefaultDailyShapeObj.GetMult(DynaVars.dblHour);
        IF PriceCurveObj<> NIL THEN PriceSignal := PriceCurveObj.GetPrice(DynaVars.dblHour);

        FOR N := 1 TO NumberOfTimes Do
        If Not SolutionAbort Then
        Begin

        // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
            Case Randomtype of
             UNIFORM:  LoadMultiplier := Random;  // number between 0 and 1
             GAUSSIAN: LoadMultiplier := Gauss(DefaultDailyShapeObj.Mean, DefaultDailyShapeObj.StdDev);
             LOGNORMAL: LoadMultiplier := QuasiLognormal(DefaultDailyShapeObj.Mean);
            End;

            SolveSnap;

            MonitorClass.SampleAll;  // Make all monitors take a sample
            EnergyMeterClass.SampleAll;  // Make all meters take a sample

            Show10PctProgress(N, NumberOfTimes);
        End
        Else
        Begin
           CmdResult := SOLUTION_ABORT;
           ErrorNumber := CmdResult;
           GlobalResult := 'Solution Aborted';
           Break;
        End;
      Finally
        MonitorClass.SaveAll;
        EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
        ProgressHide;
      End;
    End; {WITH}
End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION SolveLD1:Integer;

// Do a Daily Simulation based on a load duration curve

VAR
  N, Ndaily, i:Integer;


Begin
  Result := 0;

  WITH ActiveCircuit, ActiveCircuit.Solution Do
  Begin
    Try
      If LoadDurCurveObj = nil THEN Begin
          Dosimplemsg('Load Duration Curve Not Defined (Set LDCurve=... command). Cannot perForm solution.', 470);
          Exit;
      End;

  // Time must be set beFore entering this routine

      // MonitorClass.ResetAll;
      // EnergyMeterClass.ResetAll;

      NDaily := Round(24.0 / DynaVars.h * 3600.0);

      If Not DIFilesAreOpen Then EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

      ProgressCaption( 'Load-Duration Mode 1 Solution. ');

      // (set in Solve method) DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Year-1));

      DynaVars.intHour := 0;
      With DynaVars Do FOR i := 1 to Ndaily Do Begin

      // Set the time
        Increment_time;

        DefaultHourMult := DefaultDailyShapeObj.GetMult(dblHour);

        If    NOT SolutionAbort
        THEN  Begin
           FOR N := 1 TO LoadDurCurveObj.NumPoints Do
           Begin

              LoadMultiplier := LoadDurCurveObj.Mult(N);  // Always set LoadMultiplier with prop in case matrix must be rebuilt
              // Adjust meter interval to interval on value of present Load-Duration Curve
              IntervalHrs := LoadDurCurveObj.PresentInterval;

          // Price curve must correspond to load-duration curve
              IF PriceCurveObj<> NIL THEN PriceSignal := PriceCurveObj.Price(N);

              SolveSnap;

              MonitorClass.SampleAll;     // Make all monitors take a sample
              EnergyMeterClass.SampleAll;  // Make all meters take a sample

              EndOfTimeStepCleanup;


           End;
           ShowPctProgress((i * 100) div NDaily);
        End
        Else
        Begin
           CmdResult := SOLUTION_ABORT;
           ErrorNumber := CmdResult;
           GlobalResult := 'Solution Aborted';
           Break;
        End;

      End;
    Finally
      MonitorClass.SaveAll;
      EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
      ProgressHide;
    End;
 End; {WITH ActiveCircuit}

End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION SolveLD2:Integer;

// Hold time fixed and just vary the global load multiplier according to the global
// Load-Duration Curve

VAR
  N:Integer;

Begin
  Result := 0;

  WITH ActiveCircuit, ActiveCircuit.Solution Do
  Begin
    If LoadDurCurveObj = nil THEN Begin
        Dosimplemsg('Load Duration Curve Not Defined (Set LDCurve=... command). Cannot perForm solution.', 471);
        Exit;
    End;

// Time must be set beFore entering this routine


    // MonitorClass.ResetAll;
    // EnergyMeterClass.ResetAll;

    DefaultHourMult := DefaultDailyShapeObj.GetMult(DynaVars.dblHour);
    If Not DIFilesAreOpen Then EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

    // (set in Solve Method) DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Year-1));

    Try
      If SolutionAbort Then
      Begin
           CmdResult := SOLUTION_ABORT;
           ErrorNumber := CmdResult;
           GlobalResult := 'Solution Aborted.';
           Exit;
      End ;

      FOR N := 1 TO LoadDurCurveObj.NumPoints Do Begin

        // Adjust meter interval to interval on value of present Load-Duration Curve
        LoadMultiplier := LoadDurCurveObj.Mult(N);     // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
        IntervalHrs := LoadDurCurveObj.PresentInterval;

        // Price curve must correspond to load-duration curve
        IF PriceCurveObj<> NIL THEN PriceSignal := PriceCurveObj.Price(N);

        SolveSnap;

        MonitorClass.SampleAll;  // Make all monitors take a sample
        EnergyMeterClass.SampleAll;  // Make all meters take a sample

        EndOfTimeStepCleanup;

      End;
    Finally
      MonitorClass.SaveAll;
      EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
    End;
  End; {WITH ActiveCircuit}

End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
PROCEDURE PickAFault;
// Enable one of the faults in the circuit.  Disable the rest
VAR
   NumFaults, i, Whichone:Integer;
   FaultObj : TFaultObj;
Begin
    NumFaults := ActiveCircuit.Faults.Listsize;
    Whichone := Trunc(Random * NumFaults) + 1;
    IF   Whichone > NumFaults
    THEN Whichone := NumFaults;

    FOR i := 1 to NumFaults Do
    Begin
     FaultObj := ActiveCircuit.Faults.Get(i);
     IF   i=Whichone   THEN Begin
       ActiveFaultObj := FaultObj; // in Fault Unit
       FaultObj.Enabled := TRUE;
     End
     Else
       FaultObj.Enabled := FALSE;
    End;
End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION SolveMonteFault:Integer;

VAR
  N:Integer;

Begin
  Result := 0;

  WITH ActiveCircuit, ActiveCircuit.Solution Do
  Begin
    Try
      LoadModel := ADMITTANCE;   // All Direct solution
      LoadMultiplier := 1.0;    // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
      DynaVars.intHour := 0; DynaVars.dblHour := 0.0; // Use hour to denote Case number
      DynaVars.t := 0.0;


      // MonitorClass.ResetAll;

      ProgressCaption( 'Monte Carlo Fault Study: ' + IntToStr(NumberofTimes) + ' Different Faults.');
      ProgressCount := 0;

      SetGeneratorDispRef;

      FOR N := 1 TO NumberOfTimes Do
      IF Not SolutionAbort Then Begin
          Inc(DynaVars.intHour);
          PickAFault;  // Randomly enable one of the faults
          ActiveFaultObj.Randomize;  // Randomize the fault resistance
          SolveDirect;
          MonitorClass.SampleAll;  // Make all monitors take a sample

          Show10PctProgress(N, NumberOfTimes);
      End;
    Finally
      MonitorClass.SaveAll;
      ProgressHide;
    End;
  End;

End;
{--------------------------------------------------------------------------}
PROCEDURE AllocateAllSCParms;
Var
   i:Integer;
Begin
   WITH ActiveCircuit Do Begin
      FOR i := 1 to NumBuses Do
        Buses^[i].AllocateBusQuantities;
   End;
End;


{--------------------------------------------------------------------------}
PROCEDURE ComputeIsc;
{ Compute Isc at all buses for current values of Voc and Ysc }
Var
   i:Integer;
Begin
   WITH ActiveCircuit Do Begin
      FOR i := 1 to NumBuses Do
       WITH Buses^[i] Do Begin
          Ysc.MVMult(BusCurrent, VBus);
       End;
   End;
End;


{--------------------------------------------------------------------------}
PROCEDURE ComputeYsc(iB:integer);

{Compute YSC for I-th bus}
{Assume InjCurr is zeroed}

Var
   i,
   j,
   ref1 :Integer;

Begin
  WITH ActiveCircuit, ActiveCircuit.Solution Do begin
    WITH Buses^[iB] Do Begin
      Zsc.Clear;
      FOR i := 1 to NumNodesThisBus Do Begin
        ref1 := GetRef(i);
        IF ref1>0 Then Begin
          Currents^[ref1] := cONE;
          {SparseSet expects 1st element of voltage array, not 0-th element}
          IF   SolveSparseSet(hYsystem, @NodeV^[1], @Currents^[1]) < 1
          THEN Raise EEsolv32Problem.Create('Error Solving System Y Matrix in ComputeYsc. Problem with Sparse matrix solver.');
          {Extract Voltage Vector = column of Zsc}
          FOR j := 1 to NumNodesThisBus Do Begin
            Zsc.SetElement(j ,i, NodeV^[GetRef(j)]);
          End;
          Currents^[Ref1] :=cZERO;
        End; {IF ref...}
      End;
      Ysc.CopyFrom (Zsc);
      Ysc.invert; {Save as admittance}
    End;
  end;
End;


{--------------------------------------------------------------------------}
PROCEDURE ComputeAllYsc;
Var
   iB, j:Integer;


Begin

   WITH ActiveCircuit, ActiveCircuit.Solution Do
   Begin

     FOR j := 1 to NumNodes Do Currents^[j] := cZERO;

     ProgressCount := 0;

     FOR iB := 1 to NumBuses Do
     Begin
        ComputeYsc(iB);  // Compute YSC for iB-th Bus
        If ((iB * 10) div NumBuses) > ProgressCount Then  Begin
            Inc(ProgressCount);
            ShowPctProgress(30 + ProgressCount * 5);
        End;
     End;
   End;
End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
PROCEDURE DisableAllFaults;
Begin
   WITH ActiveCircuit Do Begin
       ActiveFaultObj := Faults.First;
       WHILE ActiveFaultObj<>Nil Do Begin
          ActiveFaultObj.Enabled := False;
          ActiveFaultObj := Faults.Next;
       End
   End;
End;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION  SolveFaultStudy:Integer;



Begin
   Result := 0;

   ShowPctProgress( 0);
   ProgressCaption( 'Computing Open-Circuit Voltages');

   With ActiveCircuit.solution Do
   Begin
      LoadModel := ADMITTANCE;
      DisableAllFaults;

      SolveDirect;   // This gets the open circuit voltages and bus lists corrected

      AllocateAllSCParms;   // Reallocate bus quantities
      UpdateVBus;  // Put present solution Voc's in bus quantities
   End;
   
   ProgressCaption ('Computing Ysc Matrices for Each Bus');
   ShowPctProgress (30);
   ComputeAllYsc;

   ProgressCaption( 'Computing Short-circuit currents.');
   ShowPctProgress (80);
   ComputeIsc;

   ShowPctProgress ( 100);
   ProgressCaption ('Done.');
   ProgressHide;
   // Now should have all we need to make a short circuit report

End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure AddFrequency(Var FreqList:pDoublearray; Var NumFreq, MaxFreq:Integer; F:Double);

{Add unique Frequency, F to list in ascending order, reallocating if necessary}

Var i, j :integer;

Begin

     {See if F is in List}

     For i := 1 to NumFreq Do Begin
         {Allow a little tolerance (0.1 hz) for the Frequency for round off error}
         If Abs(F-FreqList^[i]) < 0.1  Then Exit; // Already in List, nothing to do
     End;

     {OK, it's not in list, so let's Add it}
     Inc(NumFreq);
     If NumFreq > MaxFreq Then Begin  // Let's make a little more room
         Inc(MaxFreq, 20);
         ReallocMem(FreqList, SizeOf(FreqList^[1]) * MaxFreq);
     End;

     {Let's add it in ascending order}
     For i := 1 to NumFreq-1 Do Begin
         IF F < FreqList^[i] Then Begin
             {Push down array and insert it}
             For j := NumFreq-1 Downto i Do FreqList^[j+1] := FreqList^[j];
             FreqList^[i] := F;
             Exit;  // We're done!
         End;
     End;

     {If we fall through, tack it on to the end}
     FreqList^[NumFreq] := F;

End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Function GetSourceFrequency(pc:TPCElement):Double; // TODO - vccs has no frequency?

Var
    pVsrc:TVsourceObj;
    pIsrc:TIsourceObj;
Begin

    If Comparetext(pc.DSSClassName,'vsource')=0 Then Begin
        pVsrc := pc as TVsourceObj;
        Result := pVsrc.srcFrequency;
    End Else Begin
        pIsrc := pc as TIsourceObj;
        Result := pIsrc.srcFrequency;
    End;

End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure CollectAllFrequencies( Var FreqList:pDoubleArray; Var NumFreq:Integer);

Var
   SpectrumInUse:pIntegerArray;
   p : TPCElement;
   MaxFreq, i, j :Integer;
   pSpectrum:TSpectrumObj;
   f:Double;

Begin
    {Make a List of all frequencies in Use}

    {accumulate all unique Frequencies}
    MaxFreq := 20;    // Initial List size
    NumFreq := 0;
    Reallocmem(FreqList, Sizeof(FreqList^[1])*MaxFreq);

    WITH ActiveCircuit Do Begin
        {Check Sources -- each could have a different base frequency}
        p := Sources.First;
        WHILE p <> NIL Do Begin
              If p.Enabled then
              If SpectrumClass.Find(p.Spectrum) <> NIL Then Begin
                  pSpectrum := SpectrumClass.GetActiveObj;
                  f := GetSourceFrequency(p);
                  For j := 1 to pSpectrum.NumHarm Do Begin
                     AddFrequency(FreqList, NumFreq, MaxFreq, pSpectrum.HarmArray^[j] * f);
                  End;
              End;
              p := Sources.Next;
        End;
    End;

    {Mark Spectra being used}
        {Check loads and generators - these are assumed to be at fundamental frequency}
    SpectrumInUse := AllocMem(SizeOf(SpectruminUse^[1])*SpectrumClass.ElementCount);  //Allocate and zero
    WITH ActiveCircuit Do Begin
        p := PCelements.First;
        WHILE p <> NIL Do Begin
              If p.enabled Then
              If SpectrumClass.Find(p.Spectrum) <> NIL Then Begin
                  SpectrumInUse^[SpectrumClass.Active] := 1;
              End;
              p := PCelements.Next;
        End;
    End; {With}

    {Add marked Spectra to list}
    For i := 1 to SpectrumClass.ElementCount Do Begin
        If SpectrumInUse^[i]=1 Then Begin
            SpectrumClass.Active := i;
            pSpectrum := SpectrumClass.GetActiveObj;
            For j := 1 to pSpectrum.NumHarm Do Begin
                AddFrequency(FreqList, NumFreq, MaxFreq, pSpectrum.HarmArray^[j]*ActiveCircuit.Fundamental);
            End;
        End;
    End;

    ReallocMem(SpectrumInUse, 0);


End;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
FUNCTION  SolveHarmonic:Integer;

Var
   FrequencyList :pDoubleArray;
   i, NFreq:Integer;

Begin
   Result := 0;

   FrequencyList := NIL;   // Set up for Reallocmem
   ShowPctProgress ( 0);
   ProgressCaption( 'Performing Harmonic Solution');

   With ActiveCircuit, ActiveCircuit.solution Do
   Begin
     Try
     
       IF Frequency <> Fundamental Then Begin     // Last solution was something other than fundamental
           Frequency := Fundamental;
           IF Not RetrieveSavedVoltages THEN Exit;  {Get Saved fundamental frequency solution}
       End;

       MonitorClass.SampleAll;   // Store the fundamental frequency in the monitors

       { Get the list of Harmonic Frequencies to solve at}
       IF DoAllHarmonics THEN CollectAllFrequencies(FrequencyList, NFreq)   // Allocates FrequencyList
       ELSE Begin
                Reallocmem(FrequencyList, Sizeof(FrequencyList^[1])*HarmonicListSize);
                NFreq := HarmonicListSize;
                For i := 1 to NFreq Do FrequencyList^[i] := Fundamental * HarmonicList^[i];
            End;

       FOR i := 1 to NFreq Do Begin

           Frequency := FrequencyList^[i];
           If Abs(Harmonic - 1.0) > EPSILON THEN Begin    // Skip fundamental
               ProgressCaption ( 'Solving at Frequency = ' + Format('%-g', [Frequency]));
               ShowPctProgress ( Round((100.0*i)/Nfreq));
               SolveDirect;
               MonitorClass.SampleAll;
               // Storage devices are assumed to stay the same since there is no time variation in this mode
           End;

       End; {FOR}

       ShowPctProgress ( 100);
       ProgressCaption ( 'Done.');
     Finally
       ProgressHide;
       MonitorClass.SaveAll;
       ReallocMem(FrequencyList, 0);
     End;
     // Now should have all we need to make a short circuit report

   End;

End;
//========================================================================================
FUNCTION SolveHarmTime:Integer;     // It is based in SolveGeneralTime routine

Begin
   Result := 0;

   WITH ActiveCircuit, ActiveCircuit.Solution Do
   Begin
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
          IF Not SolutionAbort Then With DynaVars Do
          Begin
              {Compute basic multiplier from Default loadshape to use in generator dispatch, if any}
                DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);

                SolveSnap;
          //      Increment_time;  // This function is handeled from SolveHarmonics (04-10-2013)
          End;
    End;
End;
//=============================================================================
FUNCTION SolveHarmonicT:Integer;
Var
    FrequencyList :pDoubleArray;
   i, NFreq:Integer;

Begin
   Result := 0;

   FrequencyList := NIL;   // Set up for Reallocmem

   With ActiveCircuit, ActiveCircuit.solution Do
   Begin
     IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
     Try
        IF Frequency <> Fundamental Then Begin     // Last solution was something other than fundamental
           Frequency := Fundamental;
           IF Not RetrieveSavedVoltages THEN Exit;  {Get Saved fundamental frequency solution}
       End;
//     DefaultHourMult := DefaultDailyShapeObj.getmult(DynaVars.dblHour);
//     IF Load_Changed THEN Begin    //Added to update the current sources of all frequencies any time
            InitializeForHarmonics;  //the value of a load changes in a proportional way
//            Load_Changed:=FALSE;     // Added 05 dec 2013 - D. Montenegro
//     End;
       SolveSnap;
       MonitorClass.SampleAll;   // Store the fundamental frequency in the monitors
       { Get the list of Harmonic Frequencies to solve at}
       IF DoAllHarmonics THEN CollectAllFrequencies(FrequencyList, NFreq)   // Allocates FrequencyList
       ELSE Begin
                Reallocmem(FrequencyList, Sizeof(FrequencyList^[1])*HarmonicListSize);
                NFreq := HarmonicListSize;
                For i := 1 to NFreq Do FrequencyList^[i] := Fundamental * HarmonicList^[i];
            End;

       FOR i := 1 to NFreq Do Begin

           Frequency := FrequencyList^[i];
           If Abs(Harmonic - 1.0) > EPSILON THEN Begin    // Skip fundamental
//               DefaultHourMult := DefaultDailyShapeObj.getmult(DynaVars.dblHour);
               SolveHarmTime;
               MonitorClass.SampleAll;
               EndOfTimeStepCleanup;
              // Storage devices are assumed to stay the same since there is no time variation in this mode  (Not necessarelly now)
           End;
       End; {FOR}
     Increment_time;
     Finally
       MonitorClass.SaveAll;
       ReallocMem(FrequencyList, 0);
     End;
  End;

End;
end.
