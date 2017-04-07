unit DSolution;

interface

function SolutionI(mode:longint; arg: longint):longint; cdecl;
function SolutionF(mode:longint; arg: double):double; cdecl;
function SolutionS(mode:longint; arg: pAnsiChar):pAnsiChar; cdecl;
procedure SolutionV(mode:longint; out arg: OleVariant); cdecl;

implementation

uses DSSGlobals, Math, LoadShape, Utilities, YMatrix, Variants, SolutionAlgs, ExecOptions;

function SolutionI(mode:longint; arg: longint):longint; cdecl;
var
  i : Integer;
begin
  Result:=0; // Default retirn value
  case mode of
  0: begin //Solution.solve
    IF ActiveCircuit[ActiveActor] <> Nil THEN ActiveCircuit[ActiveActor].Solution.Solve(ActiveActor);
    Result:=0;
  end;
  1: begin // solution.mode - read
     //If ActiveCircuit[ActiveActor] <> Nil Then Result := GetSolutionModeID      changed to integer 8/16/00
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Mode
     Else Result := 0;
  end;
  2: begin // solution.mode - Write
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.Mode := arg; //InterpretSolveMode(Value);
  end;
  3: begin // Solution.hour - read
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.DynaVars.intHour
     Else Result := 0;
  end;
  4: begin // Solution.hour - Write
     If ActiveCircuit[ActiveActor] <> Nil Then With  ActiveCircuit[ActiveActor].Solution Do Begin
        DynaVars.intHour  := arg;
        Update_dblHour;
     End;
  end;
  5: begin  // Solution.Year - read
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Year
     Else Result := 0;
  end;
  6: begin  // Solution.Year - Write
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.Year  := arg;
  end;
  7: begin  // Solution.Iterations
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Iteration
     Else Result := 0;
  end;
  8: begin  // Solution.MaxIterations - read
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.MaxIterations
     Else Result := 0;
  end;
  9: begin  //Solution.MaxIterations - write
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.MaxIterations  := arg;
  end;
  10: begin // Solution.Number read
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.NumberOfTimes
     Else Result := 0;
  end;
  11: begin  // Solution.Number write
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.NumberOfTimes  := arg;
  end;
  12: begin  // Solution.random read
      If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.RandomType
      Else Result := 0;
  end;
  13: begin  // Solution.random write
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.RandomType := arg;
  end;
  14: begin  // Solution.Loadmodel read
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.LoadModel
    ELSE Result := 0;
  end;
  15: begin  // Solution.LoadModel Write
    If ActiveCircuit[ActiveActor] <> Nil Then  WITH ActiveCircuit[ActiveActor].Solution Do Begin
      LoadModel := arg;
      DefaultLoadModel := LoadModel;
    End;
  end;
  16: begin  // Solution.AddType read
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].AutoAddObj.AddType
     Else Result := 0;
  end;
  17: begin  // Solution.Addtype Write
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].AutoAddObj.AddType := arg;
  end;
  18: begin  // Solution.Algorithm read
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Algorithm
     Else Result := 0;
  end;
  19: begin  // Solution.Algotihm Write
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.Algorithm := arg;
  end;
  20: begin  // Solution.ControlMode read
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.ControlMode
     Else Result := 0;
  end;
  21: begin // Solution.ControlMode Write
    If ActiveCircuit[ActiveActor] <> Nil Then With ActiveCircuit[ActiveActor].Solution Do Begin
         ControlMode := arg;
         DefaultControlMode := ControlMode;
    End;
  end;
  22: begin  // Solution.ControlIterations read
     If ActiveCircuit[ActiveActor] <> Nil Then  Begin
        Result := ActiveCircuit[ActiveActor].Solution.ControlIteration;
     End;
  end;
  23: begin  // Solution.ControlIterations Write
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.ControlIteration := arg;
    End;
  end;
  24: begin  // Solution.MaxControlIterations read
      If ActiveCircuit[ActiveActor] <> Nil Then  Begin
        Result := ActiveCircuit[ActiveActor].Solution.MaxControlIterations;
     End;
  end;
  25: begin  // Solution.MaxControlIterations Write
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.MaxControlIterations := arg;
    End;
  end;
  26: begin  // Solution.Sample_docontrolactions
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.Sample_DoControlActions(ActiveActor)  ;
    End;
    Result:=0;
  end;
  27: begin // Solution.CheckFaultStatus
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.Check_Fault_Status(ActiveActor) ;
    End;
  end;
  28: begin  // Solution.SolveDirect
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
        ActiveCircuit[ActiveActor].Solution.SolveDirect(ActiveActor);
     End;
  end;
  29: begin  // Solution.SolvePFlow
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
        ActiveCircuit[ActiveActor].Solution.DoPflowSolution(ActiveActor);
     End;
  end;
  30: begin  // Solution.SolveNoControl
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
        ActiveCircuit[ActiveActor].Solution.SolveCircuit(ActiveActor);
     End;
  end;
  31: begin  // Solution.SolvePlusControl
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
        With ActiveCircuit[ActiveActor].Solution Do Begin
           SolveCircuit(ActiveActor);
           CheckControls(ActiveActor);
        End;
     End;
  end;
  32: begin  // Solution.InitSnap
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
        ActiveCircuit[ActiveActor].Solution.SnapShotInit(ActiveActor);
     End;
  end;
  33: begin  // Solution.CheckControls
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
        ActiveCircuit[ActiveActor].Solution.CheckControls(ActiveActor);
     End;
  end;
  34: begin  // Solution.SampleControlDevices
      If ActiveCircuit[ActiveActor] <> Nil Then Begin
        ActiveCircuit[ActiveActor].Solution.SampleControlDevices(ActiveActor);
     End;
  end;
  35: begin  // Solution.DoControlActions
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
        ActiveCircuit[ActiveActor].Solution.DoControlActions(ActiveActor);
     End;
  end;
  36: begin  // Solution.BuildYMatrix
      If ActiveCircuit[ActiveActor] <> Nil then  Begin
           Ymatrix.BuildYMatrix(arg, FALSE,ActiveActor)
      End;
  end;
  37: begin // Solution.SystemYChanged
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      if ActiveCircuit[ActiveActor].Solution.SystemYChanged then Result:=1
      else Result:=0;
   End;
  end;
  38: begin  // Solution.converged read
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
        Result:=0;
        if ActiveCircuit[ActiveActor].Issolved then Result:=1;
     End;
  end;
  39: begin  // Solution.converged Write
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
     if arg=1 then
     begin
       ActiveCircuit[ActiveActor].Solution.ConvergedFlag := TRUE;
       ActiveCircuit[ActiveActor].Issolved := TRUE;
     end
     else
     begin
       ActiveCircuit[ActiveActor].Solution.ConvergedFlag := FALSE;
       ActiveCircuit[ActiveActor].Issolved := FALSE;
     end;
   End;
  end;
  40: begin // Solution.TotalIterations
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Iteration
     Else Result := 0;
  end;
  41: begin  // Solution.MostIterationsDone
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.MostIterationsDone
       Else Result := 0;
  end;
  42: begin  // Solution.ControlActionsDone read
      Result:=0;
     If ActiveCircuit[ActiveActor] <> Nil Then
        if ActiveCircuit[ActiveActor].Solution.ControlActionsDone then Result := 1;
  end;
  43: begin // Solution.ControlActionsDone Write
     If ActiveCircuit[ActiveActor] <> Nil Then
        if arg=1 then
          ActiveCircuit[ActiveActor].Solution.ControlActionsDone := TRUE
        else
          ActiveCircuit[ActiveActor].Solution.ControlActionsDone := FALSE;
  end;
  44: begin // Solution.FInishTimeStep
    If ActiveCircuit[ActiveActor] <> Nil Then
    WITH ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution Do
      Begin
                MonitorClass[ActiveActor].SampleAll(ActiveActor);  // Make all monitors take a sample
                EndOfTimeStepCleanup(ActiveActor);
                Increment_time;
 //               DefaultHourMult := DefaultDailyShapeObj.getmult(TDynamicsrec.dblHour);
    End;
  end;
  45: begin  // Solution.cleanup
    If ActiveCircuit[ActiveActor] <> Nil Then
      WITH ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution Do
      Begin
                  EndOfTimeStepCleanup(ActiveActor);
      End;
  end;
  46: begin  // Solution.SolveAll
    for i := 1 to NumOfActors do
    begin
      ActiveActor :=  i;
      CmdResult   :=  DoSetCmd(1);
    end;
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
      If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Frequency
     Else Result := 0.0;
  end;
  1: begin  // Solution.Frequency Write
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.Frequency  := arg;
     Result:=0.0;
  end;
  2: begin  // Solution.Seconds read
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.dynavars.t
     Else Result := 0.0;
  end;
  3: begin  // Solution.Seconds Write
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.dynavars.t  := arg;
     Result:=0.0;
  end;
  4: begin  // Solution.Stepsize read
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.dynavars.h
     Else Result := 0.0;
  end;
  5: begin  // Solution.StepSize Write
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.dynavars.h  := arg;
     Result:=0.0;
  end;
  6: begin  // Solution.LoadMult read
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].LoadMultiplier
     Else Result := 0.0;
  end;
  7: begin  // Solution.LoadMult Write
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].LoadMultiplier  := arg;
     Result:=0.0;
  end;
  8: begin  // Solution.Convergence read
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.ConvergenceTolerance
     Else Result := 0.0;
  end;
  9: begin  // Solution.Convergence Write
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.ConvergenceTolerance  := arg;
     Result:=0.0;
  end;
  10: begin  // Solution.pctgrowth read
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN With ActiveCircuit[ActiveActor] DO
     Begin
        Result := (DefaultGrowthRate-1.0)*100.0
     End;
  end;
  11: begin  // Solution.pctGrowth Write
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN With ActiveCircuit[ActiveActor] DO
     Begin
        DefaultGrowthRate := 1.0 + arg/100.0;
        DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Solution.Year-1));
     End;
     Result:=0.0;
  end;
  12: begin  // Solution.GenkW read
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].AutoAddObj.GenkW
     Else Result := 0.0;
  end;
  13: begin  // Solution.GenkW Write
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].AutoAddObj.GenkW := arg;
     Result:=0.0;
  end;
  14: begin // Solution.GenPF read
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].AutoAddObj.GenPF
     Else Result := 0.0;
  end;
  15: begin  // Solution.GenPF Write
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].AutoAddObj.GenPF := arg;
     Result:=0.0;
  end;
  16: begin  // Solution.Capkvar read
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].AutoAddObj.Capkvar
     Else Result := 0.0;
  end;
  17: begin  // Solution.Capkvar Write
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].AutoAddObj.Capkvar := arg;
     Result:=0.0;
  end;
  18: begin  // Solution.GenMult read
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].GenMultiplier
     Else Result := 0.0;
  end;
  19: begin // Solution.GenMult Write
    If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].GenMultiplier := arg;
    Result:=0.0;
  end;
  20: begin  //Solution.dblHour read
     If ActiveCircuit[ActiveActor] <> Nil Then  Begin
        Result := ActiveCircuit[ActiveActor].Solution.DynaVars.dblHour;
     End;
  end;
  21: begin  // Solution.dblHour Write
      If ActiveCircuit[ActiveActor] <> Nil Then With ActiveCircuit[ActiveActor].Solution Do Begin
          DynaVars.intHour := Trunc(arg);
          DynaVars.dblHour := arg;
          Dynavars.t := (arg - DynaVars.intHour) * 3600.0;
      End;
  end;
  22: begin  // Solution.StepSizeMin
      If ActiveCircuit[ActiveActor] <> Nil Then Begin
          ActiveCircuit[ActiveActor].Solution.Dynavars.h := arg * 60.0;
      End;
      Result:=0.0;
  end;
  23: begin // Solution.StepSizeHr
      If ActiveCircuit[ActiveActor] <> Nil Then Begin
          ActiveCircuit[ActiveActor].Solution.Dynavars.h := arg * 3600.0;
      End;
      Result:=0.0;
  end;
  24: begin // Solution.Process_Time
      If ActiveCircuit[ActiveActor] <> Nil Then Begin
          Result  :=  ActiveCircuit[ActiveActor].Solution.Time_Solve;
      End;
  end;
  25: begin // Solution.Total_Time read
      If ActiveCircuit[ActiveActor] <> Nil Then Begin
          Result  :=  ActiveCircuit[ActiveActor].Solution.Total_Time;
      End;
  end;
  26: begin // Solution.Total_Time Write
      If ActiveCircuit[ActiveActor] <> Nil Then Begin
          ActiveCircuit[ActiveActor].Solution.Total_Time :=  arg;
      End;
  end;
  27: begin // Solution.Time_TimeStep
      If ActiveCircuit[ActiveActor] <> Nil Then Begin
          Result  :=  ActiveCircuit[ActiveActor].Solution.Time_Step;
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
    If ActiveCircuit[ActiveActor] <> Nil Then Result := pAnsiChar(AnsiString(GetSolutionModeID))
    ELSE Result := pAnsiChar(AnsiString(''));
  end;
  1: begin  // Solution.LDCurve read
     IF ActiveCircuit[ActiveActor] <> Nil Then Result := pAnsiChar(AnsiString(ActiveCircuit[ActiveActor].LoadDurCurve))
     ELSE Result := pAnsiChar(AnsiString(''));
  end;
  2: begin  // Solution.LDCurve Write
      IF ActiveCircuit[ActiveActor] <> Nil
      THEN With ActiveCircuit[ActiveActor] DO
      Begin
            LoadDurCurve    := widestring(arg);
            LoadDurCurveObj := LoadShapeClass[ActiveActor].Find(LoadDurCurve);
            IF LoadDurCurveObj=NIL THEN
             DoSimpleMsg('Load-Duration Curve not found.', 5001);
      End;
  end;
  3: begin  // Solution.DefaultDaily read
     IF   ActiveCircuit[ActiveActor] <> Nil
     THEN Result := pAnsiChar(AnsiString(ActiveCircuit[ActiveActor].DefaultDailyShapeObj.Name))
     ELSE Result := pAnsiChar(AnsiString(''));
  end;
  4: begin  // Solution.DefaultDaily Write
     If ActiveCircuit[ActiveActor] <> Nil
     Then
     Begin
           TestLoadShapeObj := LoadShapeClass[ActiveActor].Find(widestring(arg));
           IF TestLoadShapeObj <> NIL THEN ActiveCircuit[ActiveActor].DefaultDailyShapeObj  := TestLoadShapeObj;
     END;
  end;
  5: begin  // Solution.DefaultYearly read
     IF   ActiveCircuit[ActiveActor] <> Nil
     THEN Result := pAnsiChar(AnsiString(ActiveCircuit[ActiveActor].DefaultYearlyShapeObj.Name))
     ELSE Result := pAnsiChar(AnsiString(''));
  end;
  6: begin  // Solution.DefaultYearly Write
     If ActiveCircuit[ActiveActor] <> Nil
     Then
     Begin
           TestLoadShapeObj := LoadShapeClass[ActiveActor].Find(widestring(arg));
           IF TestLoadShapeObj <> NIL THEN ActiveCircuit[ActiveActor].DefaultYearlyShapeObj  := TestLoadShapeObj;
     END;
  end
  else
      Result:=pAnsiChar(AnsiString('Error, paratemer not recognized'));
  end;
end;

//**********************************Variant type properties*******************************
procedure SolutionV(mode:longint; out arg: OleVariant); cdecl;
Var i:Integer;
begin
  case mode of
  0: begin  // Solution.EventLog
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
       arg := VarArrayCreate([0, EventStrings[ActiveActor].Count-1], varOleStr);
       For i := 0 to EventStrings[ActiveActor].Count-1 Do Begin
          arg[i] := EventStrings[ActiveActor].Strings[i];
       End;
    END
    Else arg := VarArrayCreate([0,0], varOleStr);;
  end
  else
             arg[0] := 'Error, paratemer not recognized';
  end;
end;
end.
