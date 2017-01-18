unit DSolution;

interface

function SolutionI(mode:longint; arg: longint):longint; cdecl;
function SolutionF(mode:longint; arg: double):double; cdecl;
function SolutionS(mode:longint; arg: pAnsiChar):pAnsiChar; cdecl;
procedure SolutionV(mode:longint; out arg: Variant); cdecl;

implementation

uses DSSGlobals, Math, LoadShape, Utilities, YMatrix, Variants, SolutionAlgs;

function SolutionI(mode:longint; arg: longint):longint; cdecl;
begin
  Result:=0; // Default retirn value
  case mode of
  0: begin //Solution.solve
    IF ActiveCircuit <> Nil THEN ActiveCircuit.Solution.Solve;
    Result:=0;
  end;
  1: begin // solution.mode - read
     //If ActiveCircuit <> Nil Then Result := GetSolutionModeID      changed to integer 8/16/00
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Mode
     Else Result := 0;
  end;
  2: begin // solution.mode - Write
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.Mode := arg; //InterpretSolveMode(Value);
  end;
  3: begin // Solution.hour - read
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.DynaVars.intHour
     Else Result := 0;
  end;
  4: begin // Solution.hour - Write
     If ActiveCircuit <> Nil Then With  ActiveCircuit.Solution Do Begin
        DynaVars.intHour  := arg;
        Update_dblHour;
     End;
  end;
  5: begin  // Solution.Year - read
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Year
     Else Result := 0;
  end;
  6: begin  // Solution.Year - Write
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.Year  := arg;
  end;
  7: begin  // Solution.Iterations
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Iteration
     Else Result := 0;
  end;
  8: begin  // Solution.MaxIterations - read
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.MaxIterations
     Else Result := 0;
  end;
  9: begin  //Solution.MaxIterations - write
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.MaxIterations  := arg;
  end;
  10: begin // Solution.Number read
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.NumberOfTimes
     Else Result := 0;
  end;
  11: begin  // Solution.Number write
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.NumberOfTimes  := arg;
  end;
  12: begin  // Solution.random read
      If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.RandomType
      Else Result := 0;
  end;
  13: begin  // Solution.random write
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.RandomType := arg;
  end;
  14: begin  // Solution.Loadmodel read
    If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.LoadModel
    ELSE Result := 0;
  end;
  15: begin  // Solution.LoadModel Write
    If ActiveCircuit <> Nil Then  WITH ActiveCircuit.Solution Do Begin
      LoadModel := arg;
      DefaultLoadModel := LoadModel;
    End;
  end;
  16: begin  // Solution.AddType read
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.AutoAddObj.AddType
     Else Result := 0;
  end;
  17: begin  // Solution.Addtype Write
     If ActiveCircuit <> Nil Then ActiveCircuit.AutoAddObj.AddType := arg;
  end;
  18: begin  // Solution.Algorithm read
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Algorithm
     Else Result := 0;
  end;
  19: begin  // Solution.Algotihm Write
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.Algorithm := arg;
  end;
  20: begin  // Solution.ControlMode read
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.ControlMode
     Else Result := 0;
  end;
  21: begin // Solution.ControlMode Write
    If ActiveCircuit <> Nil Then With ActiveCircuit.Solution Do Begin
         ControlMode := arg;
         DefaultControlMode := ControlMode;
    End;
  end;
  22: begin  // Solution.ControlIterations read
     If ActiveCircuit <> Nil Then  Begin
        Result := ActiveCircuit.Solution.ControlIteration;
     End;
  end;
  23: begin  // Solution.ControlIterations Write
     If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.ControlIteration := arg;
    End;
  end;
  24: begin  // Solution.MaxControlIterations read
      If ActiveCircuit <> Nil Then  Begin
        Result := ActiveCircuit.Solution.MaxControlIterations;
     End;
  end;
  25: begin  // Solution.MaxControlIterations Write
    If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.MaxControlIterations := arg;
    End;
  end;
  26: begin  // Solution.Sample_docontrolactions
    If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.Sample_DoControlActions  ;
    End;
    Result:=0;
  end;
  27: begin // Solution.CheckFaultStatus
    If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.Check_Fault_Status ;
    End;
  end;
  28: begin  // Solution.SolveDirect
     If ActiveCircuit <> Nil Then Begin
        ActiveCircuit.Solution.SolveDirect;
     End;
  end;
  29: begin  // Solution.SolvePFlow
     If ActiveCircuit <> Nil Then Begin
        ActiveCircuit.Solution.DoPflowSolution;
     End;
  end;
  30: begin  // Solution.SolveNoControl
     If ActiveCircuit <> Nil Then Begin
        ActiveCircuit.Solution.SolveCircuit;
     End;
  end;
  31: begin  // Solution.SolvePlusControl
     If ActiveCircuit <> Nil Then Begin
        With ActiveCircuit.Solution Do Begin
           SolveCircuit;
           CheckControls;
        End;
     End;
  end;
  32: begin  // Solution.InitSnap
     If ActiveCircuit <> Nil Then Begin
        ActiveCircuit.Solution.SnapShotInit;
     End;
  end;
  33: begin  // Solution.CheckControls
     If ActiveCircuit <> Nil Then Begin
        ActiveCircuit.Solution.CheckControls;
     End;
  end;
  34: begin  // Solution.SampleControlDevices
      If ActiveCircuit <> Nil Then Begin
        ActiveCircuit.Solution.SampleControlDevices;
     End;
  end;
  35: begin  // Solution.DoControlActions
     If ActiveCircuit <> Nil Then Begin
        ActiveCircuit.Solution.DoControlActions;
     End;
  end;
  36: begin  // Solution.BuildYMatrix
      If ActiveCircuit <> Nil then  Begin
           Ymatrix.BuildYMatrix(arg, FALSE)
      End;
  end;
  37: begin // Solution.SystemYChanged
   If ActiveCircuit <> Nil Then Begin
      if ActiveCircuit.Solution.SystemYChanged then Result:=1
      else Result:=0;
   End;
  end;
  38: begin  // Solution.converged read
     If ActiveCircuit <> Nil Then Begin
        Result:=0;
        if ActiveCircuit.Issolved then Result:=1;
     End;
  end;
  39: begin  // Solution.converged Write
   If ActiveCircuit <> Nil Then Begin
     if arg=1 then
     begin
       ActiveCircuit.Solution.ConvergedFlag := TRUE;
       ActiveCircuit.Issolved := TRUE;
     end
     else
     begin
       ActiveCircuit.Solution.ConvergedFlag := FALSE;
       ActiveCircuit.Issolved := FALSE;
     end;
   End;
  end;
  40: begin // Solution.TotalIterations
    If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Iteration
     Else Result := 0;
  end;
  41: begin  // Solution.MostIterationsDone
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.MostIterationsDone
       Else Result := 0;
  end;
  42: begin  // Solution.ControlActionsDone read
      Result:=0;
     If ActiveCircuit <> Nil Then
        if ActiveCircuit.Solution.ControlActionsDone then Result := 1;
  end;
  43: begin // Solution.ControlActionsDone Write
     If ActiveCircuit <> Nil Then
        if arg=1 then
          ActiveCircuit.Solution.ControlActionsDone := TRUE
        else
          ActiveCircuit.Solution.ControlActionsDone := FALSE;
  end;
  44: begin // Solution.FInishTimeStep
    If ActiveCircuit <> Nil Then
    WITH ActiveCircuit, ActiveCircuit.Solution Do
      Begin
                MonitorClass.SampleAll;  // Make all monitors take a sample
                EndOfTimeStepCleanup;
                Increment_time;
 //               DefaultHourMult := DefaultDailyShapeObj.getmult(TDynamicsrec.dblHour);
    End;
  end;
  45: begin  // Solution.cleanup
    If ActiveCircuit <> Nil Then
      WITH ActiveCircuit, ActiveCircuit.Solution Do
      Begin
                  EndOfTimeStepCleanup;
      End;
  end
  else
      Result:=-1;
  end;
end;

//***************************floating point variables*******************************
function SolutionF(mode:longint; arg: double):double; cdecl;
begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // Solution.Frequency read
      If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Frequency
     Else Result := 0.0;
  end;
  1: begin  // Solution.Frequency Write
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.Frequency  := arg;
     Result:=0.0;
  end;
  2: begin  // Solution.Seconds read
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.dynavars.t
     Else Result := 0.0;
  end;
  3: begin  // Solution.Seconds Write
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.dynavars.t  := arg;
     Result:=0.0;
  end;
  4: begin  // Solution.Stepsize read
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.dynavars.h
     Else Result := 0.0;
  end;
  5: begin  // Solution.StepSize Write
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.dynavars.h  := arg;
     Result:=0.0;
  end;
  6: begin  // Solution.LoadMult read
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.LoadMultiplier
     Else Result := 0.0;
  end;
  7: begin  // Solution.LoadMult Write
     If ActiveCircuit <> Nil Then ActiveCircuit.LoadMultiplier  := arg;
     Result:=0.0;
  end;
  8: begin  // Solution.Convergence read
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.ConvergenceTolerance
     Else Result := 0.0;
  end;
  9: begin  // Solution.Convergence Write
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.ConvergenceTolerance  := arg;
     Result:=0.0;
  end;
  10: begin  // Solution.pctgrowth read
     IF ActiveCircuit <> NIL
     THEN With ActiveCircuit DO
     Begin
        Result := (DefaultGrowthRate-1.0)*100.0
     End;
  end;
  11: begin  // Solution.pctGrowth Write
     IF ActiveCircuit <> NIL
     THEN With ActiveCircuit DO
     Begin
        DefaultGrowthRate := 1.0 + arg/100.0;
        DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Solution.Year-1));
     End;
     Result:=0.0;
  end;
  12: begin  // Solution.GenkW read
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.AutoAddObj.GenkW
     Else Result := 0.0;
  end;
  13: begin  // Solution.GenkW Write
     If ActiveCircuit <> Nil Then ActiveCircuit.AutoAddObj.GenkW := arg;
     Result:=0.0;
  end;
  14: begin // Solution.GenPF read
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.AutoAddObj.GenPF
     Else Result := 0.0;
  end;
  15: begin  // Solution.GenPF Write
     If ActiveCircuit <> Nil Then ActiveCircuit.AutoAddObj.GenPF := arg;
     Result:=0.0;
  end;
  16: begin  // Solution.Capkvar read
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.AutoAddObj.Capkvar
     Else Result := 0.0;
  end;
  17: begin  // Solution.Capkvar Write
     If ActiveCircuit <> Nil Then ActiveCircuit.AutoAddObj.Capkvar := arg;
     Result:=0.0;
  end;
  18: begin  // Solution.GenMult read
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.GenMultiplier
     Else Result := 0.0;
  end;
  19: begin // Solution.GenMult Write
    If ActiveCircuit <> Nil Then ActiveCircuit.GenMultiplier := arg;
    Result:=0.0;
  end;
  20: begin  //Solution.dblHour read
     If ActiveCircuit <> Nil Then  Begin
        Result := ActiveCircuit.Solution.DynaVars.dblHour;
     End;
  end;
  21: begin  // Solution.dblHour Write
      If ActiveCircuit <> Nil Then With ActiveCircuit.Solution Do Begin
          DynaVars.intHour := Trunc(arg);
          DynaVars.dblHour := arg;
          Dynavars.t := (arg - DynaVars.intHour) * 3600.0;
      End;
  end;
  22: begin  // Solution.StepSizeMin
      If ActiveCircuit <> Nil Then Begin
          ActiveCircuit.Solution.Dynavars.h := arg * 60.0;
      End;
      Result:=0.0;
  end;
  23: begin // Solution.StepSizeHr
      If ActiveCircuit <> Nil Then Begin
          ActiveCircuit.Solution.Dynavars.h := arg * 3600.0;
      End;
      Result:=0.0;
  end;
  24: begin // Solution.Process_Time
      If ActiveCircuit <> Nil Then Begin
          Result  :=  ActiveCircuit.Solution.Time_Solve;
      End;
  end;
  25: begin // Solution.Total_Time read
      If ActiveCircuit <> Nil Then Begin
          Result  :=  ActiveCircuit.Solution.Total_Time;
      End;
  end;
  26: begin // Solution.Total_Time Write
      If ActiveCircuit <> Nil Then Begin
          ActiveCircuit.Solution.Total_Time :=  arg;
      End;
  end;
  27: begin // Solution.Time_TimeStep
      If ActiveCircuit <> Nil Then Begin
          Result  :=  ActiveCircuit.Solution.Time_Step;
      End;
  end
  else
      Result:=-1.0;
  end;
end;

//***************************String type properties*******************************
function SolutionS(mode:longint; arg: pAnsiChar):pAnsiChar; cdecl;

var
TestLoadShapeObj :TLoadShapeObj;

begin
  Result := pAnsiChar(AnsiString(''));  // Default return value
  case mode of
  0: begin  // Solution.ModeID
    If ActiveCircuit <> Nil Then Result := pAnsiChar(AnsiString(GetSolutionModeID))
    ELSE Result := pAnsiChar(AnsiString(''));
  end;
  1: begin  // Solution.LDCurve read
     IF ActiveCircuit <> Nil Then Result := pAnsiChar(AnsiString(ActiveCircuit.LoadDurCurve))
     ELSE Result := pAnsiChar(AnsiString(''));
  end;
  2: begin  // Solution.LDCurve Write
      IF ActiveCircuit <> Nil
      THEN With ActiveCircuit DO
      Begin
            LoadDurCurve    := widestring(arg);
            LoadDurCurveObj := LoadShapeClass.Find(LoadDurCurve);
            IF LoadDurCurveObj=NIL THEN
             DoSimpleMsg('Load-Duration Curve not found.', 5001);
      End;
  end;
  3: begin  // Solution.DefaultDaily read
     IF   ActiveCircuit <> Nil
     THEN Result := pAnsiChar(AnsiString(ActiveCircuit.DefaultDailyShapeObj.Name))
     ELSE Result := pAnsiChar(AnsiString(''));
  end;
  4: begin  // Solution.DefaultDaily Write
     If ActiveCircuit <> Nil
     Then
     Begin
           TestLoadShapeObj := LoadShapeClass.Find(widestring(arg));
           IF TestLoadShapeObj <> NIL THEN ActiveCircuit.DefaultDailyShapeObj  := TestLoadShapeObj;
     END;
  end;
  5: begin  // Solution.DefaultYearly read
     IF   ActiveCircuit <> Nil
     THEN Result := pAnsiChar(AnsiString(ActiveCircuit.DefaultYearlyShapeObj.Name))
     ELSE Result := pAnsiChar(AnsiString(''));
  end;
  6: begin  // Solution.DefaultYearly Write
     If ActiveCircuit <> Nil
     Then
     Begin
           TestLoadShapeObj := LoadShapeClass.Find(widestring(arg));
           IF TestLoadShapeObj <> NIL THEN ActiveCircuit.DefaultYearlyShapeObj  := TestLoadShapeObj;
     END;
  end
  else
      Result:=pAnsiChar(AnsiString('Error, paratemer not recognized'));
  end;
end;

//**********************************Variant type properties*******************************
procedure SolutionV(mode:longint; out arg: Variant); cdecl;
Var i:Integer;
begin
  case mode of
  0: begin  // Solution.EventLog
     If ActiveCircuit <> Nil Then Begin
       arg := VarArrayCreate([0, EventStrings.Count-1], varOleStr);
       For i := 0 to EventStrings.Count-1 Do Begin
          arg[i] := EventStrings.Strings[i];
       End;
    END
    Else arg := VarArrayCreate([0,0], varOleStr);;
  end
  else
             arg[0] := 'Error, paratemer not recognized';
  end;
end;
end.
