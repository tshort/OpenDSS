unit ImplSolution;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TSolution = class(TAutoObject, ISolution)
  protected
    function Get_Frequency: Double; safecall;
    function Get_Hour: Integer; safecall;
    function Get_Iterations: Integer; safecall;
    function Get_LoadMult: Double; safecall;
    function Get_MaxIterations: Integer; safecall;
    function Get_Mode: Integer; safecall;
    function Get_Number: Integer; safecall;
    function Get_Random: Integer; safecall;
    function Get_Seconds: Double; safecall;
    function Get_StepSize: Double; safecall;
    function Get_Tolerance: Double; safecall;
    function Get_Year: Integer; safecall;
    procedure Set_Frequency(Value: Double); safecall;
    procedure Set_Hour(Value: Integer); safecall;
    procedure Set_LoadMult(Value: Double); safecall;
    procedure Set_MaxIterations(Value: Integer); safecall;
    procedure Set_Mode(Mode: Integer); safecall;
    procedure Set_Number(Value: Integer); safecall;
    procedure Set_Random(Random: Integer); safecall;
    procedure Set_Seconds(Value: Double); safecall;
    procedure Set_StepSize(Value: Double); safecall;
    procedure Set_Tolerance(Value: Double); safecall;
    procedure Set_Year(Value: Integer); safecall;
    procedure Solve; safecall;
    function Get_ModeID: WideString; safecall;
    function Get_LoadModel: Integer; safecall;
    procedure Set_LoadModel(Value: Integer); safecall;
    function Get_LDCurve: WideString; safecall;
    procedure Set_LDCurve(const Value: WideString); safecall;
    function Get_pctGrowth: Double; safecall;
    procedure Set_pctGrowth(Value: Double); safecall;
    function Get_AddType: Integer; safecall;
    procedure Set_AddType(Value: Integer); safecall;
    function Get_GenkW: Double; safecall;
    procedure Set_GenkW(Value: Double); safecall;
    function Get_GenPF: Double; safecall;
    procedure Set_GenPF(Value: Double); safecall;
    function Get_Capkvar: Double; safecall;
    procedure Set_Capkvar(Value: Double); safecall;
    function Get_Algorithm: Integer; safecall;
    procedure Set_Algorithm(Value: Integer); safecall;
    function Get_ControlMode: Integer; safecall;
    procedure Set_ControlMode(Value: Integer); safecall;
    function Get_GenMult: Double; safecall;
    procedure Set_GenMult(Value: Double); safecall;
    function Get_DefaultDaily: WideString; safecall;
    function Get_DefaultYearly: WideString; safecall;
    procedure Set_DefaultDaily(const Value: WideString); safecall;
    procedure Set_DefaultYearly(const Value: WideString); safecall;
    function Get_EventLog: OleVariant; safecall;
    function Get_dblHour: Double; safecall;
    procedure Set_dblHour(Value: Double); safecall;
    procedure Set_StepsizeHr(Value: Double); safecall;
    procedure Set_StepsizeMin(Value: Double); safecall;
    function Get_ControlIterations: Integer; safecall;
    function Get_MaxControlIterations: Integer; safecall;
    procedure Sample_DoControlActions; safecall;
    procedure Set_ControlIterations(Value: Integer); safecall;
    procedure Set_MaxControlIterations(Value: Integer); safecall;
    procedure CheckFaultStatus; safecall;
    procedure SolveDirect; safecall;
    procedure SolveNoControl; safecall;
    procedure SolvePflow; safecall;
    procedure SolvePlusControl; safecall;
    procedure SolveSnap; safecall;
    procedure CheckControls; safecall;
    procedure InitSnap; safecall;
    function Get_SystemYChanged: WordBool; safecall;
    procedure BuildYMatrix(BuildOption, AllocateVI: Integer); safecall;
    procedure DoControlActions; safecall;
    procedure SampleControlDevices; safecall;
    function Get_Converged: WordBool; safecall;
    procedure Set_Converged(Value: WordBool); safecall;
    function Get_Totaliterations: Integer; safecall;
    function Get_MostIterationsDone: Integer; safecall;
    function Get_ControlActionsDone: WordBool; safecall;
    procedure Set_ControlActionsDone(Value: WordBool); safecall;
    procedure Cleanup; safecall;
    procedure FinishTimeStep; safecall;
    function Get_Process_Time: Double; safecall;
    function Get_Total_Time: Double; safecall;
    procedure Set_Total_Time(Value: Double); safecall;
    function Get_Time_of_Step: Double; safecall;
    function Get_IntervalHrs: Double; safecall;
    procedure Set_IntervalHrs(Value: Double); safecall;
    procedure SolveAll; safecall;
  end;

implementation

uses ComServ, DSSGlobals, Math, LoadShape, Utilities, YMatrix, Variants, SolutionAlgs, Solution, ExecOptions;

function TSolution.Get_Frequency: Double;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Frequency
     Else Result := 0.0;
end;

function TSolution.Get_Hour: Integer;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.DynaVars.intHour
     Else Result := 0;
end;

function TSolution.Get_Iterations: Integer;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Iteration
     Else Result := 0;
end;

function TSolution.Get_LoadMult: Double;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].LoadMultiplier
     Else Result := 0.0;
end;

function TSolution.Get_MaxIterations: Integer;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.MaxIterations
     Else Result := 0;
end;

function TSolution.Get_Mode: Integer;
begin
     //If ActiveCircuit <> Nil Then Result := GetSolutionModeID      changed to integer 8/16/00
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Mode
     Else Result := 0;
end;

function TSolution.Get_Number: Integer;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.NumberOfTimes
     Else Result := 0;
end;

function TSolution.Get_Random: Integer;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.RandomType
     Else Result := 0;
end;

function TSolution.Get_Seconds: Double;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.dynavars.t
     Else Result := 0.0;
end;

function TSolution.Get_StepSize: Double;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.dynavars.h
     Else Result := 0.0;
end;

function TSolution.Get_Tolerance: Double;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.ConvergenceTolerance
     Else Result := 0.0;
end;

function TSolution.Get_Year: Integer;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Year
     Else Result := 0;
end;

procedure TSolution.Set_Frequency(Value: Double);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.Frequency  := Value;
end;

procedure TSolution.Set_Hour(Value: Integer);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then With  ActiveCircuit[ActiveActor].Solution Do Begin
        DynaVars.intHour  := Value;
        Update_dblHour;
     End;
end;

procedure TSolution.Set_LoadMult(Value: Double);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].LoadMultiplier  := Value;
end;

procedure TSolution.Set_MaxIterations(Value: Integer);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.MaxIterations  := Value;
end;

procedure TSolution.Set_Mode(Mode: Integer);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.Mode := Mode; //InterpretSolveMode(Value);
end;

procedure TSolution.Set_Number(Value: Integer);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.NumberOfTimes  := Value;
end;

procedure TSolution.Set_Random(Random: Integer);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.RandomType := Random;
end;

procedure TSolution.Set_Seconds(Value: Double);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.dynavars.t  := Value;
end;

procedure TSolution.Set_StepSize(Value: Double);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
         ActiveCircuit[ActiveActor].Solution.dynavars.h  := Value;
         Set_IntervalHrs(Value);     // Keep IntervalHrs in synch with time step size
     End;
end;

procedure TSolution.Set_Tolerance(Value: Double);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.ConvergenceTolerance  := Value;
end;

procedure TSolution.Set_Year(Value: Integer);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.Year  := Value;
end;

procedure TSolution.Solve;
begin
  IF ActiveCircuit[ActiveActor] <> Nil THEN ActiveCircuit[ActiveActor].Solution.Solve(ActiveActor);
end;

function TSolution.Get_ModeID: WideString;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := GetSolutionModeID
    ELSE Result := '';
end;

function TSolution.Get_LoadModel: Integer;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.LoadModel
    ELSE Result := 0;
end;

procedure TSolution.Set_LoadModel(Value: Integer);
begin

   If ActiveCircuit[ActiveActor] <> Nil Then  WITH ActiveCircuit[ActiveActor].Solution Do Begin
      LoadModel := Value;
      DefaultLoadModel := LoadModel;
   End;

end;

function TSolution.Get_LDCurve: WideString;
begin
     IF ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].LoadDurCurve
     ELSE Result := '';
end;

procedure TSolution.Set_LDCurve(const Value: WideString);
begin
      IF ActiveCircuit[ActiveActor] <> Nil
      THEN With ActiveCircuit[ActiveActor] DO
      Begin
            LoadDurCurve    := Value;
            LoadDurCurveObj := LoadShapeClass[ActiveActor].Find(LoadDurCurve);
            IF LoadDurCurveObj=NIL THEN
             DoSimpleMsg('Load-Duration Curve not found.', 5001);
      End;

end;

function TSolution.Get_pctGrowth: Double;
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN With ActiveCircuit[ActiveActor] DO
     Begin
        Result := (DefaultGrowthRate-1.0)*100.0
     End;
end;

procedure TSolution.Set_pctGrowth(Value: Double);
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN With ActiveCircuit[ActiveActor] DO
     Begin
        DefaultGrowthRate := 1.0 + Value/100.0;
        DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Solution.Year-1));
     End;
end;

function TSolution.Get_AddType: Integer;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].AutoAddObj.AddType
     Else Result := 0;
end;

procedure TSolution.Set_AddType(Value: Integer);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].AutoAddObj.AddType := Value;
end;

function TSolution.Get_GenkW: Double;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].AutoAddObj.GenkW
     Else Result := 0.0;
end;

procedure TSolution.Set_GenkW(Value: Double);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].AutoAddObj.GenkW := Value;
end;

function TSolution.Get_GenPF: Double;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].AutoAddObj.GenPF
     Else Result := 0.0;
end;

procedure TSolution.Set_GenPF(Value: Double);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].AutoAddObj.GenPF := Value;
end;

function TSolution.Get_Capkvar: Double;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].AutoAddObj.Capkvar
     Else Result := 0.0;
end;

procedure TSolution.Set_Capkvar(Value: Double);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].AutoAddObj.Capkvar := Value;
end;

function TSolution.Get_Algorithm: Integer;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Algorithm
     Else Result := 0;
end;

procedure TSolution.Set_Algorithm(Value: Integer);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.Algorithm := Value;
end;

function TSolution.Get_ControlMode: Integer;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.ControlMode
     Else Result := 0;
end;

procedure TSolution.Set_ControlMode(Value: Integer);
begin
    If ActiveCircuit[ActiveActor] <> Nil Then With ActiveCircuit[ActiveActor].Solution Do Begin
         ControlMode := Value;
         DefaultControlMode := ControlMode;
    End;
end;

function TSolution.Get_GenMult: Double;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].GenMultiplier
     Else Result := 0.0;
end;

procedure TSolution.Set_GenMult(Value: Double);
begin
    If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].GenMultiplier := Value;
end;

function TSolution.Get_DefaultDaily: WideString;
begin
     IF   ActiveCircuit[ActiveActor] <> Nil
     THEN Result := ActiveCircuit[ActiveActor].DefaultDailyShapeObj.Name
     ELSE Result := '';
end;

function TSolution.Get_DefaultYearly: WideString;
begin
     IF   ActiveCircuit[ActiveActor] <> Nil
     THEN Result := ActiveCircuit[ActiveActor].DefaultYearlyShapeObj.Name
     ELSE Result := '';
end;

procedure TSolution.Set_DefaultDaily(const Value: WideString);
Var  TestLoadShapeObj :TLoadShapeObj;
begin
     If ActiveCircuit[ActiveActor] <> Nil
     Then
     Begin
           TestLoadShapeObj := LoadShapeClass[ActiveActor].Find(Value);
           IF TestLoadShapeObj <> NIL THEN ActiveCircuit[ActiveActor].DefaultDailyShapeObj  := TestLoadShapeObj;
     END;
end;

procedure TSolution.Set_DefaultYearly(const Value: WideString);
Var  TestLoadShapeObj :TLoadShapeObj;
begin
     If ActiveCircuit[ActiveActor] <> Nil
     Then
     Begin
           TestLoadShapeObj := LoadShapeClass[ActiveActor].Find(Value);
           IF TestLoadShapeObj <> NIL THEN ActiveCircuit[ActiveActor].DefaultYearlyShapeObj  := TestLoadShapeObj;
     END;

end;

function TSolution.Get_EventLog: OleVariant;
Var i:Integer;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
       Result := VarArrayCreate([0, EventStrings[ActiveActor].Count-1], varOleStr);
       For i := 0 to EventStrings[ActiveActor].Count-1 Do Begin
          Result[i] := EventStrings[ActiveActor].Strings[i];
       End;
    END
    Else Result := VarArrayCreate([0,0], varOleStr);;

end;

function TSolution.Get_dblHour: Double;
begin
     If ActiveCircuit <> Nil Then  Begin
        Result := ActiveCircuit[ActiveActor].Solution.DynaVars.dblHour;
     End;
end;

procedure TSolution.Set_dblHour(Value: Double);
begin
  If ActiveCircuit[ActiveActor] <> Nil Then With ActiveCircuit[ActiveActor].Solution Do Begin
      DynaVars.intHour := Trunc(Value);
      DynaVars.dblHour := Value;
      Dynavars.t := (Value - DynaVars.intHour) * 3600.0;
  End;
end;

procedure TSolution.Set_StepsizeHr(Value: Double);
begin
  If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.Dynavars.h := Value * 3600.0;
  End;
end;

procedure TSolution.Set_StepsizeMin(Value: Double);
begin

  If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.Dynavars.h := Value * 60.0;
  End;

end;

function TSolution.Get_ControlIterations: Integer;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then  Begin
        Result := ActiveCircuit[ActiveActor].Solution.ControlIteration;
     End;
end;

function TSolution.Get_MaxControlIterations: Integer;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then  Begin
        Result := ActiveCircuit[ActiveActor].Solution.MaxControlIterations;
     End;
end;

procedure TSolution.Sample_DoControlActions;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.Sample_DoControlActions(ActiveActor)  ;
   End;
end;

procedure TSolution.Set_ControlIterations(Value: Integer);
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.ControlIteration := Value;
    End;
end;

procedure TSolution.Set_MaxControlIterations(Value: Integer);
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.MaxControlIterations := Value;
    End;
end;

procedure TSolution.CheckFaultStatus;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.Check_Fault_Status(ActiveActor);
   End;
end;

procedure TSolution.SolveDirect;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.SolveDirect(ActiveActor);
   End;
end;

procedure TSolution.SolveNoControl;
{Solves without checking controls}
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.SolveCircuit(ActiveActor);
   End;
end;

procedure TSolution.SolvePflow;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.DoPflowSolution(ActiveActor);
   End;
end;

procedure TSolution.SolvePlusControl;
{One Pass Through the solution and then dispatches controls}
begin
   If ActiveCircuit <> Nil Then Begin
      With ActiveCircuit[ActiveActor].Solution Do Begin
         SolveCircuit(ActiveActor);
         CheckControls(ActiveActor);
      End;
   End;
end;

procedure TSolution.SolveSnap;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.SolveSnap(ActiveActor);
   End;
end;

procedure TSolution.CheckControls;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.CheckControls(ActiveActor);
   End;
end;

procedure TSolution.InitSnap;
{Initi some things that are done at the beginning of a snapshot solve}
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.SnapShotInit(ActiveActor);
   End;
end;

function TSolution.Get_SystemYChanged: WordBool;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      Result := ActiveCircuit[ActiveActor].Solution.SystemYChanged;
   End;
end;

procedure TSolution.BuildYMatrix(BuildOption, AllocateVI: Integer);
{
  Build Options
    1 = Series elements only
    2 = Whole Y matrix

  AllocateVI
    TRUE:  Reallocate VI
    FALSE: Do not Reallocate VI; leave as is
}
begin
  If ActiveCircuit[ActiveActor] <> Nil then  Begin
    If AllocateVI = 0 then
       Ymatrix.BuildYMatrix(BuildOption, FALSE, ActiveActor)
    else
       Ymatrix.BuildYMatrix(BuildOption, TRUE, ActiveActor)
  End;
end;

procedure TSolution.DoControlActions;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.DoControlActions(ActiveActor);
   End;
end;

procedure TSolution.SampleControlDevices;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.SampleControlDevices(ActiveActor);
   End;
end;

function TSolution.Get_Converged: WordBool;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      Result := ActiveCircuit[ActiveActor].Issolved;
   End;
end;

procedure TSolution.Set_Converged(Value: WordBool);

{Set the flag directly to force its setting}
begin

   If ActiveCircuit[ActiveActor] <> Nil Then Begin
     ActiveCircuit[ActiveActor].Solution.ConvergedFlag := Value;
     ActiveCircuit[ActiveActor].Issolved := Value;
   End;
end;

function TSolution.Get_Totaliterations: Integer;

// Same as Iterations interface

begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Iteration
     Else Result := 0;
end;

function TSolution.Get_MostIterationsDone: Integer;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.MostIterationsDone
     Else Result := 0;
end;

function TSolution.Get_ControlActionsDone: WordBool;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.ControlActionsDone;
end;

procedure TSolution.Set_ControlActionsDone(Value: WordBool);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.ControlActionsDone := Value;
end;

procedure TSolution.Cleanup;
begin
    If ActiveCircuit <> Nil Then
    WITH ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution Do
      Begin
                EndOfTimeStepCleanup(ActiveActor);
    End;
end;

procedure TSolution.FinishTimeStep;
begin
    If ActiveCircuit <> Nil Then
    WITH ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution Do
      Begin
                MonitorClass[ActiveActor].SampleAll(ActiveActor);  // Make all monitors take a sample
                EndOfTimeStepCleanup(ActiveActor);
                Increment_time;
 //               DefaultHourMult := DefaultDailyShapeObj.getmult(TDynamicsrec.dblHour);
    End;
end;

function TSolution.Get_Process_Time: Double;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Time_Solve;
end;

function TSolution.Get_Total_Time: Double;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Total_Time;
end;

procedure TSolution.Set_Total_Time(Value: Double);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.Total_Time   :=  Value;
end;

function TSolution.Get_Time_of_Step: Double;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Time_Step;
end;

function TSolution.Get_IntervalHrs: Double;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.IntervalHrs;
end;

procedure TSolution.Set_IntervalHrs(Value: Double);
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.IntervalHrs := Value;
end;

procedure TSolution.SolveAll;
var
  i : Integer;
begin
  for i := 1 to NumOfActors do
  begin
    ActiveActor :=  i;
    CmdResult   :=  DoSetCmd(1);
  end;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TSolution, Class_Solution, ciInternal, tmApartment);
  IsMultiThread := True;
end.
