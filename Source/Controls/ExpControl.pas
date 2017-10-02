unit ExpControl;

{
  ----------------------------------------------------------
  Copyright (c) 2015-2016, University of Pittsburgh
  All rights reserved.
  ----------------------------------------------------------

  Notes: adapted and simplified from InvControl for adaptive controller research
}

INTERFACE

  uses
    {$IFDEF FPC}gqueue{$ELSE}System.Generics.Collections{$ENDIF}, Command,
    ControlClass, ControlElem, CktElement, DSSClass, PVSystem, Arraydef, ucomplex,
    utilities, Dynamics, PointerList, Classes, StrUtils;

  type
    TExpControl = class(TControlClass)
      protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const ExpControlName:String):Integer;Override;
      public
        constructor Create;
        destructor Destroy; override;

        FUNCTION Edit:Integer; override;     // uses global parser
        FUNCTION NewObject(const ObjName:String):Integer; override;
        PROCEDURE UpdateAll;
    end;

    TExpControlObj = class(TControlElem)
      private
            ControlActionHandle: Integer;
            ControlledElement: Array of TPVSystemObj;    // list of pointers to controlled PVSystem elements
            MonitoredElement : TDSSCktElement;  // First PVSystem element for now

            // PVSystemList information
            FListSize:Integer;
            FPVSystemNameList:TStringList;
            FPVSystemPointerList:PointerList.TPointerList;

            // working storage for each PV system under management
            FPriorVpu: Array of Double;
            FPresentVpu: Array of Double;
            FPendingChange: Array of Integer;
            FVregs: Array of Double;
            FPriorQ: Array of Double;
            FTargetQ: Array of Double;
            FWithinTol: Array of Boolean;

            // temp storage for biggest PV system, not each one
            cBuffer : Array of Complex;

            // user-supplied parameters (also PVSystemList and EventLog)
            FVregInit: Double;
            FSlope: Double;
            FVregTau: Double;
            FQbias: Double;
            FVregMin: Double;
            FVregMax: Double;
            FQmaxLead: Double;
            FQmaxLag: Double;
            FdeltaQ_factor: Double;
            FVoltageChangeTolerance: Double; // hard-wire now?
            FVarChangeTolerance: Double;     // hard-wire now?

            PROCEDURE Set_PendingChange(Value: Integer;DevIndex: Integer);
            FUNCTION  Get_PendingChange(DevIndex: Integer):Integer;
            FUNCTION  ReturnElementsList:String;
            PROCEDURE UpdateExpControl(i:integer);
    protected
            PROCEDURE Set_Enabled(Value:Boolean);Override;
    public

            constructor Create(ParClass:TDSSClass; const ExpControlName:String);
            destructor  Destroy; override;

            PROCEDURE   MakePosSequence; Override;  // Make a positive Sequence Model
            PROCEDURE   RecalcElementData; Override;
            PROCEDURE   CalcYPrim; Override;    // Always Zero for an ExpControl

            // Sample control quantities and set action times in Control Queue
            PROCEDURE   Sample;  Override;

            // Do the action that is pending from last sample
            PROCEDURE   DoPendingAction(Const Code, ProxyHdl:Integer); Override;

            PROCEDURE   Reset; Override;  // Reset to initial defined state

            PROCEDURE   GetInjCurrents(Curr: pComplexArray); Override;
            PROCEDURE   GetCurrents(Curr: pComplexArray); Override;

            PROCEDURE   InitPropertyValues(ArrayOffset:Integer);Override;
            PROCEDURE   DumpProperties(Var F:TextFile; Complete:Boolean);Override;

            FUNCTION    MakePVSystemList:Boolean;
            FUNCTION    GetPropertyValue(Index:Integer):String;Override;

            Property    PendingChange[DevIndex: Integer]:Integer Read Get_PendingChange Write Set_PendingChange;

   end;

  VAR
    ActiveExpControlObj:TExpControlObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, Sysutils, DSSClassDefs, DSSGlobals, Circuit,  uCmatrix, MathUtil, Math;

CONST

    NumPropsThisClass = 11;

    NONE = 0;
    CHANGEVARLEVEL = 1;

{--------------------------------------------------------------------------}
constructor TExpControl.Create;  // Creates superstructure for all ExpControl objects
Begin
  Inherited Create;
  Class_name   := 'ExpControl';
  DSSClassType := DSSClassType + EXP_CONTROL;
  DefineProperties;
  CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
  CommandList.Abbrev := TRUE;
End;

{--------------------------------------------------------------------------}
destructor TExpControl.Destroy;

Begin

     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TExpControl.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;

     // Define Property names
     PropertyName[1] := 'PVSystemList';
     PropertyName[2] := 'Vreg';
     PropertyName[3] := 'Slope';
     PropertyName[4] := 'VregTau';
     PropertyName[5] := 'Qbias';
     PropertyName[6] := 'VregMin';
     PropertyName[7] := 'VregMax';
     PropertyName[8] := 'QmaxLead';
     PropertyName[9] := 'QmaxLag';
     PropertyName[10] := 'EventLog';
     PropertyName[11] := 'DeltaQ_factor';

     PropertyHelp[1] := 'Array list of PVSystems to be controlled.'+CRLF+CRLF+
                        'If not specified, all PVSystems in the circuit are assumed to be controlled by this ExpControl.';
     PropertyHelp[2] := 'Per-unit voltage at which reactive power is zero; defaults to 1.0.'+CRLF+CRLF+
                        'This may dynamically self-adjust when VregTau > 0, limited by VregMin and VregMax.'+
                        'If imput as 0, Vreg will be initialized from a snapshot solution with no inverter Q.'+
                        'The equilibrium point of reactive power is also affected by Qbias';
     PropertyHelp[3] := 'Per-unit reactive power injection / per-unit voltage deviation from Vreg; defaults to 50.'+CRLF+CRLF+
                        'Unlike InvControl, base reactive power is constant at the inverter kva rating.';
     PropertyHelp[4] := 'Time constant for adaptive Vreg. Defaults to 1200 seconds.'+CRLF+CRLF+
                        'When the control injects or absorbs reactive power due to a voltage deviation from the Q=0 crossing of the volt-var curve, '+
                        'the Q=0 crossing will move toward the actual terminal voltage with this time constant. '+
                        'Over time, the effect is to gradually bring inverter reactive power to zero as the grid voltage changes due to non-solar effects. '+
                        'If zero, then Vreg stays fixed';
     PropertyHelp[5] := 'Equilibrium per-unit reactive power when V=Vreg; defaults to 0.'+CRLF+CRLF+
                        'Enter > 0 for lagging (capacitive) bias, < 0 for leading (inductive) bias.';
     PropertyHelp[6] := 'Lower limit on adaptive Vreg; defaults to 0.95 per-unit';
     PropertyHelp[7] := 'Upper limit on adaptive Vreg; defaults to 1.05 per-unit';
     PropertyHelp[8] := 'Limit on leading (inductive) reactive power injection, in per-unit of base kva; defaults to 0.44.'+
                        'For Category A inverters per P1547/D7, set this value to 0.25.'+CRLF+CRLF+
                        'Regardless of QmaxLead, the reactive power injection is still '+
                        'limited by dynamic headroom when actual real power output exceeds 0%';
     PropertyHelp[9] := 'Limit on lagging (capacitive) reactive power injection, in per-unit of base kva; defaults to 0.44.'+CRLF+CRLF+
                        'For Category A inverters per P1547/D7, set this value to 0.25.'+
                        'Regardless of QmaxLag, the reactive power injection is still '+
                        'limited by dynamic headroom when actual real power output exceeds 0%';
     PropertyHelp[10] := '{Yes/True* | No/False} Default is No for ExpControl. Log control actions to Eventlog.';
     PropertyHelp[11] := 'Convergence parameter; Defaults to 0.7. '+CRLF+CRLF+
                         'Sets the maximum change (in per unit) from the prior var output level to the desired var output level during each control iteration. '+
                         'If numerical instability is noticed in solutions such as var sign changing from one control iteration to the next and voltages oscillating between two values with some separation, '+
                         'this is an indication of numerical instability (use the EventLog to diagnose). '+
                         'If the maximum control iterations are exceeded, and no numerical instability is seen in the EventLog of via monitors, then try increasing the value of this parameter to reduce the number '+
                         'of control iterations needed to achieve the control criteria, and move to the power flow solution.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list
End;

{--------------------------------------------------------------------------}
FUNCTION TExpControl.NewObject(const ObjName:String):Integer;
Begin
    // Make a new ExpControl and add it to ExpControl class list
    WITH ActiveCircuit Do
    Begin
      ActiveCktElement := TExpControlObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

{--------------------------------------------------------------------------}
FUNCTION TExpControl.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;


Begin
  ActiveExpControlObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveExpControlObj;
  Result := 0;
  WITH ActiveExpControlObj Do Begin
    ParamPointer := 0;
    ParamName := Parser.NextParam;
    Param := Parser.StrValue;
    WHILE Length(Param)>0 Do Begin
      IF Length(ParamName) = 0 THEN Inc(ParamPointer)
      ELSE ParamPointer := CommandList.GetCommand(ParamName);

      If (ParamPointer>0) and (ParamPointer<=NumProperties)
      THEN PropertyValue[ParamPointer]:= Param;

      CASE ParamPointer OF
        0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 364);
        1: begin
           InterpretTStringListArray(Param, FPVSystemNameList);
           FPVSystemPointerList.Clear; // clear this for resetting on first sample
           FListSize := FPVSystemNameList.count;
           end;
        2: If Parser.DblValue >= 0 then FVregInit := Parser.DblValue;
        3: If Parser.DblValue > 0 then FSlope := Parser.DblValue;
        4: If Parser.DblValue >= 0 then FVregTau := Parser.DblValue; // zero means fixed Vreg
        5: FQbias := Parser.DblValue;
        6: If Parser.DblValue > 0 then FVregMin := Parser.DblValue;
        7: If Parser.DblValue > 0 then FVregMax := Parser.DblValue;
        8: If Parser.DblValue >= 0 then FQmaxLead := Parser.DblValue;
        9: If Parser.DblValue >= 0 then FQmaxLag := Parser.DblValue;
       10: ShowEventLog := InterpretYesNo(param);
       11: FdeltaQ_factor := Parser.DblValue;
      ELSE
        // Inherited parameters
        ClassEdit( ActiveExpControlObj, ParamPointer - NumPropsthisClass)
      End;
      ParamName := Parser.NextParam;
      Param := Parser.StrValue;
    End;
    RecalcElementData;
  End;
End;

FUNCTION TExpControl.MakeLike(const ExpControlName:String):Integer;
VAR
   OtherExpControl:TExpControlObj;
   i, j:Integer;
Begin
   Result := 0;
   {See if we can find this ExpControl name in the present collection}
   OtherExpControl := Find(ExpControlName);
   IF OtherExpControl<>Nil THEN
   WITH ActiveExpControlObj Do Begin

      NPhases := OtherExpControl.Fnphases;
      NConds  := OtherExpControl.Fnconds; // Force Reallocation of terminal stuff

      for i := 1 to FPVSystemPointerList.ListSize do begin
        ControlledElement[i]       := OtherExpControl.ControlledElement[i];
        FWithinTol[i]              := OtherExpControl.FWithinTol[i];
      end;

      FListSize                  := OtherExpControl.FListSize;
      FVoltageChangeTolerance    := OtherExpControl.FVoltageChangeTolerance;
      FVarChangeTolerance        := OtherExpControl.FVarChangeTolerance;
      FVregInit                  := OtherExpControl.FVregInit;
      FSlope                     := OtherExpControl.FSlope;
      FVregTau                   := OtherExpControl.FVregTau;
      FQbias                     := OtherExpControl.FQbias;
      FVregMin                   := OtherExpControl.FVregMin;
      FVregMax                   := OtherExpControl.FVregMax;
      FQmaxLead                  := OtherExpControl.FQmaxLead;
      FQmaxLag                   := OtherExpControl.FQmaxLag;
      FdeltaQ_factor             := OtherExpControl.FdeltaQ_factor;
      For j := 1 to ParentClass.NumProperties Do PropertyValue[j] := OtherExpControl.PropertyValue[j];

   End
   ELSE  DoSimpleMsg('Error in ExpControl MakeLike: "' + ExpControlName + '" Not Found.', 370);

End;

{==========================================================================}
{                    TExpControlObj                                        }
{==========================================================================}

constructor TExpControlObj.Create(ParClass:TDSSClass; const ExpControlName:String);

Begin
     Inherited Create(ParClass);
     Name                     := LowerCase(ExpControlName);
     DSSObjType               := ParClass.DSSClassType;

     ElementName              := '';

     {
       Control elements are zero current sources that attach to a terminal of a
       power-carrying device, but do not alter voltage or current flow.
       Define a default number of phases and conductors here and update in
       RecalcElementData routine if necessary. This allocates arrays for voltages
       and currents and gives more direct access to the values, if needed
     }
     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms  := 1;  // this forces allocation of terminals and conductors
     // This general feature should not be used for ExpControl,
     // because it controls more than one PVSystem

     ShowEventLog       := FALSE;

     ControlledElement        := nil;
     FPVSystemNameList        := nil;
     FPVSystemPointerList     := nil;
     cBuffer                  := nil;
     FPriorVpu                := nil;
     FPresentVpu              := nil;
     FPendingChange           := nil;
     FPriorQ                  := nil;
     FTargetQ                 := nil;
     FWithinTol               := nil;

     FVoltageChangeTolerance  :=0.0001;  // per-unit
     FVarChangeTolerance      :=0.0001;  // per-unit

     FPVSystemNameList := TSTringList.Create;
     FPVSystemPointerList := PointerList.TPointerList.Create(20);  // Default size and increment

     // user parameters for dynamic Vreg
     FVregInit := 1.0; // 0 means to find it during initialization
     FSlope := 50.0;
     FVregTau := 1200.0;
     FVregs := nil;
     FQbias := 0.0;
     FVregMin := 0.95;
     FVregMax := 1.05;
     FQmaxLead := 0.44;
     FQmaxLag := 0.44;
     FdeltaQ_factor := 0.7; // only on control iterations, not the final solution

     //generic for control
     FPendingChange         := nil;

     InitPropertyValues(0);
End;

destructor TExpControlObj.Destroy;
Begin
     ElementName := '';
     Finalize(ControlledElement);
     Finalize(cBuffer);
     Finalize(FPriorVpu);
     Finalize(FPresentVpu);
     Finalize(FPendingChange);
     Finalize(FPriorQ);
     Finalize(FTargetQ);
     Finalize(FWithinTol);
     Finalize(FVregs);
     Inherited Destroy;
End;

PROCEDURE TExpControlObj.RecalcElementData;
VAR
   i      :Integer;
   maxord :Integer;
Begin
    IF FPVSystemPointerList.ListSize = 0 Then  MakePVSystemList;

    IF FPVSystemPointerList.ListSize > 0  Then begin
    {Setting the terminal of the ExpControl device to same as the 1st PVSystem element}
         MonitoredElement :=  TDSSCktElement(FPVSystemPointerList.Get(1));   // Set MonitoredElement to 1st PVSystem in lise
         Setbus(1, MonitoredElement.Firstbus);
    End;

    maxord := 0; // will be the size of cBuffer
    for i := 1 to FPVSystemPointerList.ListSize do begin
        // User ControlledElement[] as the pointer to the PVSystem elements
         ControlledElement[i] :=  TPVSystemObj(FPVSystemPointerList.Get(i));  // pointer to i-th PVSystem
         Nphases := ControlledElement[i].NPhases;  // TEMC TODO - what if these are different sizes (same concern exists with InvControl)
         Nconds  := Nphases;
         if (ControlledElement[i] = nil) then
            DoErrorMsg('ExpControl: "' + Self.Name + '"',
              'Controlled Element "' + FPVSystemNameList.Strings[i-1] + '" Not Found.',
              ' PVSystem object must be defined previously.', 361);
         if ControlledElement[i].Yorder > maxord then maxord := ControlledElement[i].Yorder;
         ControlledElement[i].ActiveTerminalIdx := 1; // Make the 1 st terminal active
    end;
    if maxord > 0 then SetLength(cBuffer, SizeOF(Complex) * maxord);
End;

procedure TExpControlObj.MakePosSequence;
// ***  This assumes the PVSystem devices have already been converted to pos seq
begin
  IF FPVSystemPointerList.ListSize = 0 Then  RecalcElementData;
  // TEMC - from here to inherited was copied from InvControl
  Nphases := 3;
  Nconds := 3;
  Setbus(1, MonitoredElement.GetBus(ElementTerminal));
  IF FPVSystemPointerList.ListSize > 0  Then Begin
    {Setting the terminal of the ExpControl device to same as the 1st PVSystem element}
    { This sets it to a realistic value to avoid crashes later }
    MonitoredElement :=  TDSSCktElement(FPVSystemPointerList.Get(1));   // Set MonitoredElement to 1st PVSystem in lise
    Setbus(1, MonitoredElement.Firstbus);
    Nphases := MonitoredElement.NPhases;
    Nconds := Nphases;
  End;
  inherited;
end;

PROCEDURE TExpControlObj.CalcYPrim;
Begin
End;

PROCEDURE TExpControlObj.GetCurrents(Curr: pComplexArray);
Var
   i:Integer;
Begin
// Control is a zero current source
  For i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

PROCEDURE TExpControlObj.GetInjCurrents(Curr: pComplexArray);
Var
   i:Integer;
Begin
// Control is a zero current source
  For i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

PROCEDURE TExpControlObj.DumpProperties(Var F:TextFile; Complete:Boolean);
VAR
   i:Integer;
Begin
    Inherited DumpProperties(F,Complete);

    WITH ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    If Complete THEN
    Begin
      Writeln(F);
    End;
End;

PROCEDURE TExpControlObj.DoPendingAction;
VAR
  i             :Integer;
  Qset, DeltaQ  :Double;
  Qmaxpu, Qpu   :Double;
  Qbase         :Double;
  Qinvmaxpu     :Double;
  PVSys         :TPVSystemObj;
BEGIN
  for i := 1 to FPVSystemPointerList.ListSize do begin
    PVSys := ControlledElement[i];   // Use local variable in loop
    if PendingChange[i] = CHANGEVARLEVEL then begin
      PVSys.VWmode := FALSE;
      PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
      PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar
      FTargetQ[i] := 0.0;
      Qbase  := PVSys.kVARating;
      Qinvmaxpu := PVSys.kvarLimit / Qbase;
      Qpu := PVSys.Presentkvar / Qbase; // no change for now

      if (FWithinTol[i]=False) then begin
        // look up Qpu from the slope crossing at Vreg, and add the bias
        Qpu := -FSlope * (FPresentVpu[i] - FVregs[i]) + FQbias;
        If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name+','+PVSys.Name,
          Format(' Setting Qpu= %.5g at FVreg= %.5g, Vpu= %.5g', [Qpu, FVregs[i],FPresentVpu[i]]));
      end;

      // apply limits on Qpu, then define the target in kVAR
      Qmaxpu := Sqrt(1 - Sqr(PVSys.PresentkW/Qbase)); // dynamic headroom
      if Qmaxpu > Qinvmaxpu then Qmaxpu := Qinvmaxpu;
      if Abs(Qpu) > Qmaxpu then Qpu := QmaxPu * Sign(Qpu);
      if Qpu < -FQmaxLead then Qpu := -FQmaxLead;
      if Qpu > FQmaxLag then Qpu := FQmaxLag;
      FTargetQ[i] := Qbase * Qpu;

      // only move the non-bias component by deltaQ_factor in this control iteration
      DeltaQ := FTargetQ[i] - FPriorQ[i];
      Qset := FPriorQ[i] + DeltaQ * FdeltaQ_factor;
 //     Qset := FQbias * Qbase;
      If PVSys.Presentkvar <> Qset Then PVSys.Presentkvar := Qset;
      If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name +','+ PVSys.Name,
                             Format(' Setting PVSystem output kvar= %.5g',
                             [PVSys.Presentkvar]));
      FPriorQ[i] := Qset;
      FPriorVpu[i] := FPresentVpu[i];
      ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
      // Force recalc of power parms
      Set_PendingChange(NONE,i);
    end
  end;

end;

PROCEDURE TExpControlObj.Sample;
VAR
  i,j                 :Integer;
  basekV, Vpresent    :Double;
  Verr, Qerr          :Double;
  PVSys               :TPVSystemObj;
begin
  // If list is not defined, go make one from all PVSystem in circuit
  IF FPVSystemPointerList.ListSize=0 Then RecalcElementData;

  If (FListSize>0) then Begin
    // If an ExpControl controls more than one PV, control each one
    // separately based on the PVSystem's terminal voltages, etc.
    for i := 1 to FPVSystemPointerList.ListSize do begin
      PVSys := ControlledElement[i];   // Use local variable in loop
      // Calculate the present average voltage  magnitude
      PVSys.ComputeVTerminal;
      for j := 1 to PVSys.Yorder do cBuffer[j] := PVSys.Vterminal^[j];
      BasekV := ActiveCircuit.Buses^[PVSys.terminals^[1].busRef].kVBase;
      Vpresent := 0;
      For j := 1 to PVSys.NPhases Do Vpresent := Vpresent + Cabs(cBuffer[j]);
      FPresentVpu[i] := (Vpresent / PVSys.NPhases) / (basekV * 1000.0);
      // if initializing with Vreg=0 in static mode, we want to FIND Vreg
      if (ActiveCircuit.Solution.ControlMode = CTRLSTATIC) and (FVregInit <= 0.0) then
        FVregs[i] := FPresentVpu[i];
      // both errors are in per-unit
      Verr := Abs(FPresentVpu[i] - FPriorVpu[i]);
      Qerr := Abs(PVSys.Presentkvar - FTargetQ[i]) / PVSys.kVARating;
      // process the sample
      if (PVSys.InverterON = FALSE) and (PVSys.VarFollowInverter = TRUE) then begin // not injecting
        if (FVregTau > 0.0) and (FVregs[i] <= 0.0) then
          FVregs[i] := FPresentVpu[i]; // wake up to the grid voltage, otherwise track it while not injecting
        continue;
      end;
      PVSys.VWmode := FALSE;
      if ((Verr > FVoltageChangeTolerance) or (Qerr > FVarChangeTolerance) or
          (ActiveCircuit.Solution.ControlIteration = 1)) then begin
        FWithinTol[i] := False;
        Set_PendingChange(CHANGEVARLEVEL,i);
        With  ActiveCircuit.Solution.DynaVars Do
          ControlActionHandle := ActiveCircuit.ControlQueue.Push (intHour, t + TimeDelay, PendingChange[i], 0, Self);
        If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name+' '+PVSys.Name, Format
          (' outside Hit Tolerance, Verr= %.5g, Qerr=%.5g', [Verr,Qerr]));
      end else begin
        FWithinTol[i] := True;
        If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name+' '+PVSys.Name, Format
          (' within Hit Tolerance, Verr= %.5g, Qerr=%.5g', [Verr,Qerr]));
      end;
    end;  {For}
  end; {If FlistSize}
end;

procedure TExpControlObj.InitPropertyValues(ArrayOffset: Integer);
begin
  PropertyValue[1]  := '';      // PVSystem list
  PropertyValue[2]  := '1';     // initial Vreg
  PropertyValue[3]  := '50';    // slope
  PropertyValue[4] := '1200.0'; // VregTau
  PropertyValue[5]  := '0';     // Q bias
  PropertyValue[6]  := '0.95';  // Vreg min
  PropertyValue[7]  := '1.05';  // Vreg max
  PropertyValue[8]  := '0.44';     // Qmax leading
  PropertyValue[9]  := '0.44';     // Qmax lagging
  PropertyValue[10] := 'no';    // write event log?
  PropertyValue[11] := '0.7';   // DeltaQ_factor
  inherited  InitPropertyValues(NumPropsThisClass);
end;

Function TExpControlObj.MakePVSystemList:Boolean;
VAR
   PVSysClass:TDSSClass;
   PVSys:TPVsystemObj;
   i:Integer;
begin
  Result := FALSE;
  PVSysClass := GetDSSClassPtr('PVsystem');
  If FListSize > 0 Then Begin    // Name list is defined - Use it
    SetLength(ControlledElement,FListSize+1);  // Use this as the main pointer to PVSystem Elements
    SetLength(FPriorVpu, FListSize+1);
    SetLength(FPresentVpu, FListSize+1);
    SetLength(FPendingChange,FListSize+1);
    SetLength(FPriorQ,FListSize+1);
    SetLength(FTargetQ,FListSize+1);
    SetLength(FWithinTol, FListSize+1);
    SetLength(FVregs, FListSize+1);
    For i := 1 to FListSize Do Begin
      PVSys := PVSysClass.Find(FPVSystemNameList.Strings[i-1]);
      If Assigned(PVSys) and PVSys.Enabled Then FPVSystemPointerList.New := PVSys;
    End;
  End Else Begin
     {Search through the entire circuit for enabled pvsysten objects and add them to the list}
         For i := 1 to PVSysClass.ElementCount Do Begin
            PVSys :=  PVSysClass.ElementList.Get(i);
            If PVSys.Enabled Then FPVSystemPointerList.New := PVSys;
            FPVSystemNameList.Add(PVSys.Name);
         End;
         FListSize := FPVSystemPointerList.ListSize;

         SetLength(ControlledElement,FListSize+1);

         SetLength(FPriorVpu, FListSize+1);
         SetLength(FPresentVpu, FListSize+1);

         SetLength(FPendingChange,FListSize+1);
         SetLength(FPriorQ,FListSize+1);
         SetLength(FTargetQ,FListSize+1);
         SetLength(FWithinTol, FListSize+1);
         SetLength(FVregs, FListSize+1);
    End;  {Else}

  //Initialize arrays
  For i := 1 to FlistSize Do begin
//    PVSys := PVSysClass.Find(FPVSystemNameList.Strings[i-1]);
//    Set_NTerms(PVSys.NTerms); // TODO - what is this for?
    FPriorVpu[i] := 0.0;
    FPresentVpu[i] := 0.0;
    FPriorQ[i] := -1.0;
    FTargetQ[i] :=0.0;
    FWithinTol[i] := False;
    FVregs[i] := FVregInit;
    FPendingChange[i] := NONE;
  end; {For}
  RecalcElementData;
  If FPVSystemPointerList.ListSize>0 Then Result := TRUE;
end;

procedure TExpControlObj.Reset;
begin
  // inherited;
end;

FUNCTION TExpControlObj.GetPropertyValue(Index: Integer): String;
Begin
  Result := '';
  CASE Index of
    1   : Result := ReturnElementsList;
    2   : Result := Format('%.6g', [FVregInit]);
    3   : Result := Format('%.6g', [FSlope]);
    4   : Result := Format('%.6g', [FVregTau]);
    5   : Result := Format('%.6g', [FQbias]);
    6   : Result := Format('%.6g', [FVregMin]);
    7   : Result := Format('%.6g', [FVregMax]);
    8   : Result := Format('%.6g', [FQmaxLead]);
    9   : Result := Format('%.6g', [FQmaxLag]);
    11  : Result := Format('%.6g', [FdeltaQ_factor]);
    // 10 skipped, EventLog always went to the default handler
  ELSE  // take the generic handler
    Result := Inherited GetPropertyValue(index);
  END;
End;

FUNCTION TExpControlObj.ReturnElementsList: String;
VAR
  i :Integer;
Begin
  If FListSize=0 Then Begin
    Result := '';
    Exit;
  End;
  Result := '['+ FPVSystemNameList.Strings[0];
  For i := 1 to FListSize-1 Do Begin
    Result := Result + ', ' + FPVSystemNameList.Strings[i];
  End;
  Result := Result + ']';  // terminate the array
End;

procedure TExpControlObj.Set_Enabled(Value: Boolean);
begin
  inherited;
  {Reset controlled PVSystems to original PF}
end;

procedure TExpControlObj.Set_PendingChange(Value: Integer;DevIndex: Integer);
begin
  FPendingChange[DevIndex] := Value;
  DblTraceParameter := Value;
end;

procedure TExpControlObj.UpdateExpControl(i:integer);
Var
  j                      : Integer;
  PVSys                  : TPVSystemObj;
  dt, Verr: Double; // for DYNAMICVREG
begin
  for j := 1 to FPVSystemPointerList.ListSize do begin
    PVSys := ControlledElement[j];
    if FVregTau > 0.0 then begin
      dt :=  ActiveCircuit.Solution.Dynavars.h;
      Verr := FPresentVpu[j] - FVregs[j];
      FVregs[j] := FVregs[j] + Verr * (1 - Exp (-dt / FVregTau));
    end else begin
      Verr := 0.0;
    end;
    if FVregs[j] < FVregMin then FVregs[j] := FVregMin;
    if FVregs[j] > FVregMax then FVregs[j] := FVregMax;
    PVSys.Set_Variable(5,FVregs[j]);
    If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name+','+PVSys.Name,
      Format(' Setting new Vreg= %.5g Vpu=%.5g Verr=%.5g',
      [FVregs[j], FPresentVpu[j], Verr]));
  end;
end;

FUNCTION TExpControlObj.Get_PendingChange(DevIndex: Integer):Integer;
begin
  Result := FPendingChange[DevIndex];
end;

//Called at end of main power flow solution loop
PROCEDURE TExpControl.UpdateAll;
VAR
  i : Integer;
Begin
  For i := 1 to ElementList.ListSize  Do
    With TExpControlObj(ElementList.Get(i)) Do
      If Enabled Then UpdateExpControl(i);
End;

INITIALIZATION

Finalization

end.
