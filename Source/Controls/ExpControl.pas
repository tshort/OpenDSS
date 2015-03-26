unit ExpControl;

{
  ----------------------------------------------------------
  Copyright (c) 2015, University of Pittsburgh
  All rights reserved.
  ----------------------------------------------------------

  Notes: adapted and simplified from InvControl for adaptive controller research
}

INTERFACE

USES
     System.Generics.Collections, Command, ControlClass, ControlElem,
     CktElement, DSSClass, PVSystem, Arraydef, ucomplex,
     utilities, Dynamics, PointerList, Classes, StrUtils;

type

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
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

   // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TExpControlObj = class(TControlElem)
     private
            ControlActionHandle: Integer;
            ControlledElement: Array of TPVSystemObj;    // list of pointers to controlled PVSystem elements
            MonitoredElement : TDSSCktElement;  // First PVSystem element for now

            // PVSystemList information
            FkWLimit,
            FkvarLimit,
            FkVALimit,
            FVref,  // kV rating for the PVSystem object
            FPpf,  // power factor parameter from the PVSystem object, not necessarily present pf 'output' if limited by kva rating or other parameters
            Fpresentkvar, // kvar parameter from the PVSystem object, not necessarily present kvar output if limited by kva rating or other parameters
            FpresentkW: Array of Double;
            NPhasesPVSys: Array of Integer;
            NCondsPVSys: Array of Integer;
            FListSize:Integer;
            FPVSystemNameList:TStringList;
            FPVSystemPointerList:PointerList.TPointerList;

            // TODO - revisit whether we need these, especially large cBuffer if it's temporary
            cBuffer : Array of Array of Complex;    // Complexarray buffer
            CondOffset : Array of Integer; // Offset for monitored terminal

            // TODO - check whether we really need all of these
            FAvgpVuPrior: Array of Double;
            FPresentVpu: Array of Double;
            FPendingChange: Array of Integer;
            FVregs: Array of Double;
            QDeliver: Array of Double;
            QNew: Array of Double;
            QOld: Array of Double;
            PNew: Array of Double;
            POld: Array of Double;
            QHeadRoom: Array of Double;
            Qoutputpu: Array of Double;
            Qdesiredpu: Array of Double;
            FVpuSolution: Array of Array of Double;
            FVpuSolutionIdx: Integer;

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

            // convergence testing - keep these
            FVoltageChangeTolerance: Double; // hard-wire now?
            FVarChangeTolerance: Double;     // hard-wire now?
            FWithinTol: Array of Boolean;

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

            PROCEDURE   GetCurrents(Curr: pComplexArray); Override; // Get present value of terminal Curr
            PROCEDURE   GetInjCurrents(Curr: pComplexArray); Override;   // Returns Injection currents

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
                        'This may self-adjust when VregTau > 0, limited by VregMin and VregMax'+CRLF+
                        'The equilibrium point of reactive power is also affected by Qbias';
     PropertyHelp[3] := 'Per-unit reactive power injection / per-unit voltage deviation from Vreg; defaults to 50.'+CRLF+CRLF+
                        'Unlike InvControl, base reactive power is constant.'+CRLF+
                        'Base Q = sqrt(kva+kva - kw*kw)';
     PropertyHelp[4] := 'Time constant for adaptive Vreg. Defaults to 1200 seconds.'+CRLF+CRLF+
                        'When the control injects or absorbs reactive power due to a voltage deviation from the Q=0 crossing of vvc_curve1,'+CRLF+
                        'the Q=0 crossing will move toward the actual terminal voltage with this time constant.'+CRLF+
                        'Over time, the effect is to gradually bring inverter reactive power to zero as the grid voltage changes due to non-solar effects.'+CRLF+
                        'If zero, then Vreg stays fixed';
     PropertyHelp[5] := 'Equilibrium per-unit reactive power when V=Vreg; defaults to 0.';
     PropertyHelp[6] := 'Lower limit on adaptive Vreg; defaults to 0.95 per-unit';
     PropertyHelp[7] := 'Upper limit on adaptive Vreg; defaults to 1.05 per-unit';
     PropertyHelp[8] := 'Limit on leading (inductive) reactive power injection, in per-unit of Base Q; defaults to 1.'+CRLF+CRLF+
                        'See help on Slope for definition of Base Q. Even if QmaxLead > 1, the reactive power injection is still '+CRLF+
                        'limited by dynamic headroom; i.e. the inverter can only absorb more than Base Q when the'+CRLF+
                        'actual real power output is less than 100%';
     PropertyHelp[9] := 'Limit on lagging (capacitive) reactive power injection, in per-unit of Base Q; defaults to 1.'+CRLF+CRLF+
                        'See help on Slope for definition of Base Q. Even if QmaxLag > 1, the reactive power injection is still '+CRLF+
                        'limited by dynamic headroom; i.e. the inverter can only provide more than Base Q when the'+CRLF+
                        'actual real power output is less than 100%';
     PropertyHelp[10] := '{Yes/True* | No/False} Default is No for ExpControl. Log control actions to Eventlog.';
     PropertyHelp[11] := 'Convergence parameter; Defaults to 0.7. '+CRLF+CRLF+
                         'Sets the maximum change (in per unit) from the prior var output level to the desired var output level during each control iteration. '+CRLF+
                         'If numerical instability is noticed in solutions such as var sign changing from one control iteration to the next and voltages oscillating between two values with some separation, '+
                         'this is an indication of numerical instability (use the EventLog to diagnose). '+CRLF+
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
        2: If Parser.DblValue > 0 then FVregInit := Parser.DblValue;
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
      for i := 1 to FPVSystemPointerList.ListSize DO
      begin

        ControlledElement[i]       := OtherExpControl.ControlledElement[i];

        FkWLimit[i]                := OtherExpControl.FkWLimit[i];
        FkvarLimit[i]              := OtherExpControl.FkvarLimit[i];
        FkVALimit[i]               := OtherExpControl.FkVALimit[i];
        FVref[i]                   := OtherExpControl.FVref[i];
        FPpf[i]                    := OtherExpControl.FPpf[i];
        Fpresentkvar[i]            := OtherExpControl.Fpresentkvar[i];
        FpresentkW[i]              := OtherExpControl.FpresentkW[i];

        CondOffset[i]              := OtherExpControl.CondOffset[i];
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

     ControlledElement        := nil;
     FkWLimit                 := nil;
     FkvarLimit               := nil;
     FkVALimit                := nil;
     FVref                    := nil;
     FPpf                     := nil;
     Fpresentkvar             := nil;
     FpresentkW               := nil;
     NPhasesPVSys             := nil;
     NCondsPVSys              := nil;
     FPVSystemNameList        := nil;
     FPVSystemPointerList     := nil;
     cBuffer                  := nil;
     CondOffset               := nil;

     // volt-watt, only related variables
     FAvgpVuPrior             := nil;
     FPresentVpu              := nil;

     FPendingChange           := nil;

      // following apply to volt-var only
     QDeliver                 := nil;
     QNew                     := nil;
     QOld                     := nil;
     QHeadRoom                := nil;
     PNew                     := nil;
     POld                     := nil;

     FVpuSolution             := nil;
     FVpuSolutionIdx          := 0;
     Qoutputpu                := nil;
     Qdesiredpu               := nil;
     FVoltageChangeTolerance  :=0.0001;
     FVarChangeTolerance      :=0.025;

     FWithinTol               := nil;

     FPVSystemNameList := TSTringList.Create;
     FPVSystemPointerList := PointerList.TPointerList.Create(20);  // Default size and increment

     // user parameters for dynamic Vreg
     FVregInit := 1.0;
     FSlope := 50.0;
     FVregTau := 1200.0;
     FVregs := nil;
     FQbias := 0.0;
     FVregMin := 0.95;
     FVregMax := 1.05;
     FQmaxLead := 1.0;
     FQmaxLag := 1.0;
     FdeltaQ_factor := 0.7;

     //generic for control
     FPendingChange         := nil;

     InitPropertyValues(0);
   //  RecalcElementData;
End;

destructor TExpControlObj.Destroy;
Begin
     ElementName := '';
     Finalize(ControlledElement);
     Finalize(FkWLimit);
     Finalize(FkvarLimit);
     Finalize(FkVALimit);
     Finalize(FVref);
     Finalize(FPpf);
     Finalize(Fpresentkvar);
     Finalize(FpresentkW);
     Finalize(NPhasesPVSys);
     Finalize(NCondsPVSys);
     Finalize(cBuffer);
     Finalize(CondOffset);
     Finalize(FAvgpVuPrior);
     Finalize(FPresentVpu);

     Finalize(FPendingChange);

     Finalize(QDeliver);
     Finalize(QNew);
     Finalize(QOld);
     Finalize(QHeadroom);
     Finalize(Qoutputpu);
     Finalize(Qdesiredpu);
     Finalize(PNew);
     Finalize(POld);
     Finalize(FVpuSolution);
     Finalize(FWithinTol);

     Finalize(FVregs);

     Inherited Destroy;
End;

PROCEDURE TExpControlObj.RecalcElementData;
VAR
   i      :Integer;
Begin
    IF FPVSystemPointerList.ListSize = 0 Then  MakePVSystemList;

    IF FPVSystemPointerList.ListSize > 0  Then
    {Setting the terminal of the ExpControl device to same as the 1st PVSystem element}
    Begin
         MonitoredElement :=  TDSSCktElement(FPVSystemPointerList.Get(1));   // Set MonitoredElement to 1st PVSystem in lise
         Setbus(1, MonitoredElement.Firstbus);
    End;

    for i := 1 to FPVSystemPointerList.ListSize do
    begin

        // User ControlledElement[] as the pointer to the PVSystem elements
         ControlledElement[i] :=  TPVSystemObj(FPVSystemPointerList.Get(i));  // pointer to i-th PVSystem

         SetLength(cBuffer[i], SizeOF(Complex) * ControlledElement[i].Yorder );

         ControlledElement[i].ActiveTerminalIdx := 1; // Make the 1 st terminal active

         if (ControlledElement[i] <> Nil) then
         With ControlledElement[i] Do
         begin
            FkVALimit[i]    := kVARating;
            FVref[i]        := PresentkV;
            FkWLimit[i]     := Pmpp; // AC
            FkvarLimit[i]   := kVARating;  // can output vars up to the kva limit of the inverter
            FPpf[i]         := PowerFactor;
            Fpresentkvar[i] := Presentkvar;
            FpresentkW[i]   := PresentkW;
            NPhasesPVSys[i] := NPhases;
            NCondsPVSys[i]  := NConds;
            CondOffset[i]   := (NTerms-1) * NCondsPVSys[i]; // for speedy sampling
         end
         else
         begin
            ControlledElement[i] := nil; // PVSystem element not found
            DoErrorMsg('ExpControl: "' + Self.Name + '"',
              'Controlled Element "' + FPVSystemNameList.Strings[i-1] + '" Not Found.',
              ' PVSystem object must be defined previously.', 361);
        end;
    end;

End;

procedure TExpControlObj.MakePosSequence;

// ***  This assumes the PVSystem devices have already been converted to pos seq

var
  i : Integer;
  LocalDSCktElement : TDSSCktElement;

begin
    IF FPVSystemPointerList.ListSize = 0 Then  RecalcElementData;

    for i := 1 to FPVSystemPointerList.ListSize do
    begin

       LocalDSCktElement := TDSSCktelement(FPVSystemPointerList.Get(i));
       if LocalDSCktElement <> Nil then
       begin
          NphasesPVSys[i] := LocalDSCktElement.NPhases;
          NcondsPVSys[i]  := LocalDSCktElement.NConds;
       end;
  end;
  inherited;
end;

PROCEDURE TExpControlObj.CalcYPrim;
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;

PROCEDURE TExpControlObj.GetCurrents(Curr: pComplexArray);
VAR
   i,j:Integer;
Begin

  for j := 1 to FPVSystemPointerList.ListSize do
  begin
      For i := 1 to NCondsPVSys[j] Do Curr^[i + j*NCondsPVSys[j]] := CZERO;
  end;

End;

PROCEDURE TExpControlObj.GetInjCurrents(Curr: pComplexArray);
Var i,j:Integer;
Begin
  for j := 1 to FPVSystemPointerList.ListSize do
  begin
     FOR i := 1 to NCondsPVSys[j] Do Curr^[i + j*NCondsPVSys[j]] := CZERO;
  end;
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
  i                  :Integer;
  SMonitoredElement  :Complex;
  Qpresentpu, DeltaQ :Double;
 // local pointer to current PVSystem element
  PVSys              :TPVSystemObj;
  FDiffvar           :Array of Double;
  FDesiredpu_temp    :Array of Double;
BEGIN
  SetLength(FDiffvar,4+1);
  SetLength(FDesiredpu_temp, 4+1);

  for i := 1 to FPVSystemPointerList.ListSize do begin
    PVSys := ControlledElement[i];   // Use local variable in loop
    SMonitoredElement := PVSys.Power[1]; // s is in va
    if PendingChange[i] = CHANGEVARLEVEL then begin
      PVSys.VWmode := FALSE;
      PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
      PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar
      QDesiredpu[i] := 0.0;
      QHeadRoom[i] := SQRT(Sqr(PVSys.kVARating)-Sqr(PVSys.PresentkW));
      QPresentpu   := PVSys.Presentkvar / QHeadRoom[i]; // TODO - apply Q bias and limits

      // look up the value from the volt-var slope and intercept
      if (FWithinTol[i]=False) then begin
        if FVregs[i] <= 0.0 then begin // initialize this PVSystem's Vreg if needed
          FVregs[i] := FVregInit;
          PVSys.Set_Variable(5,FVregs[i]);
        end;
        // look up Q from the curve, but enter with a voltage adjusted for moving Vreg
        Qdesiredpu[i] := -FSlope * (FPresentVpu[i] - FVregs[i]);
        If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name+','+PVSys.Name+',',
          Format('  FVreg= %.5g, Vpu= %.5g', [FVregs[i],FPresentVpu[i]]));
      end;

      // only move deltaQ_factor amount to the desired p.u. available var output
      QDeliver[i] := QDesiredpu[i]*QHeadRoom[i];
      DeltaQ := QDeliver[i] - Qold[i];
      QNew[i] := QOld[i] + DeltaQ * FdeltaQ_factor;
      If PVSys.Presentkvar <> Qnew[i] Then PVSys.Presentkvar := Qnew[i];
      Qoutputpu[i] := PVSys.Presentkvar / QHeadroom[i];
      If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name +','+ PVSys.Name+',',
                             Format('**%s mode set PVSystem output var level to**, kvar= %.5g',
                             [GetPropertyValue(2), PVSys.Presentkvar,FPresentVpu[i]]));
      FAvgpVuPrior[i] := FPresentVpu[i];
      QOld[i] := QNew[i];
      ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
      // Force recalc of power parms
      Set_PendingChange(NONE,i);
    end
  end;
  Finalize(FDiffvar);
  Finalize(FDesiredpu_temp);
end;

{--------------------------------------------------------------------------}
PROCEDURE TExpControlObj.Sample;

VAR
   i,j                   :Integer;
   basekV,
   Vpresent              :Double;

begin
  // If list is not defined, go make one from all PVSystem in circuit
  IF FPVSystemPointerList.ListSize=0 Then   RecalcElementData;

  If (FListSize>0) then Begin
    // If an ExpControl controls more than one PV, control each one
    // separately based on the PVSystem's terminal voltages, etc.
    for i := 1 to FPVSystemPointerList.ListSize do begin
      // Calculate the present average voltage  magnitude
      ControlledElement[i].ComputeVTerminal;
      for j := 1 to ControlledElement[i].Yorder do cBuffer[i,j] := ControlledElement[i].Vterminal^[j];
      BasekV := ActiveCircuit.Buses^[ ControlledElement[i].terminals^[1].busRef].kVBase;
      Vpresent := 0;
      For j := 1 to ControlledElement[i].NPhases Do Vpresent := Vpresent + Cabs(cBuffer[i,j]);
      FPresentVpu[i] := (Vpresent / ControlledElement[i].NPhases) / (basekV * 1000.0);

      // process the sample
      if (ControlledElement[i].InverterON = FALSE) and (ControlledElement[i].VarFollowInverter = TRUE) then exit;                    ControlledElement[i].VWmode := FALSE;
      if (FWithinTol[i] = False) then begin
        if (((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance) or
          ((Abs(Abs(Qoutputpu[i]) - Abs(Qdesiredpu[i])) > FVarChangeTolerance))) or
          (ActiveCircuit.Solution.ControlIteration = 1)) then begin
          FWithinTol[i] := False;
          Set_PendingChange(CHANGEVARLEVEL,i);
          With  ActiveCircuit.Solution.DynaVars Do
            ControlActionHandle := ActiveCircuit.ControlQueue.Push (intHour, t + TimeDelay, PendingChange[i], 0, Self);
          If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name+' '+ControlledElement[i].Name, Format
            ('**Ready to change var output due in %s mode**, Vavgpu= %.5g, VPriorpu=%.5g',
            [GetPropertyValue(2), FPresentVpu[i],FAvgpVuPrior[i]]));
        end else begin
          if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) <= FVoltageChangeTolerance) and
            ((Abs(Abs(Qoutputpu[i]) - Abs(Qdesiredpu[i])) <= FVarChangeTolerance))) then FWithinTol[i] := True;
          If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name+' '+ControlledElement[i].Name, Format
            ('**Hit Tolerance**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));

        end;
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
  PropertyValue[8]  := '1';     // Qmax leading
  PropertyValue[9]  := '1';     // Qmax lagging
  PropertyValue[10] := 'no';    // write event log?
  PropertyValue[11] := '0.7';   // DeltaQ_factor
  inherited  InitPropertyValues(NumPropsThisClass);
end;

Function TExpControlObj.MakePVSystemList:Boolean;
VAR
   PVSysClass:TDSSClass;
   PVSys:TPVsystemObj;
   i,j:Integer;
begin
  Result := FALSE;
  PVSysClass := GetDSSClassPtr('PVsystem');
  If FListSize > 0 Then Begin    // Name list is defined - Use it
    SetLength(CondOffset,FListSize+1);
    SetLength(cBuffer,FListSize+1,7);  // assuming no more than 6 conductors
    SetLength(ControlledElement,FListSize+1);  // Use this as the main pointer to PVSystem Elements
    SetLength(FkWLimit,FListSize+1);
    SetLength(FkVALimit,FListSize+1);
    SetLength(FkvarLimit,FListSize+1);
    SetLength(FVref,FListSize+1);
    SetLength(FPpf,FListSize+1);
    SetLength(Fpresentkvar,FListSize+1);
    SetLength(FpresentkW,FListSize+1);
    SetLength(FAvgpVuPrior, FListSize+1);
    SetLength(FPresentVpu, FListSize+1);
    SetLength(NPhasesPVSys,FListSize+1);
    SetLength(NCondsPVSys,FListSize+1);
    SetLength(FPendingChange,FListSize+1);
    SetLength(QDeliver,FListSize+1);
    SetLength(QNew,FListSize+1);
    SetLength(QOld,FListSize+1);
    SetLength(QHeadroom,FListSize+1);
    SetLength(Qoutputpu,FListSize+1);
    SetLength(Qdesiredpu,FListSize+1);
    SetLength(PNew,FListSize+1);
    SetLength(POld,FListSize+1);
    SetLength(FVpuSolution,FListSize+1,2+1);
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

         SetLength(FkWLimit,FListSize+1);
         SetLength(FkVALimit,FListSize+1);
         SetLength(FkvarLimit,FListSize+1);
         SetLength(FVref,FListSize+1);
         SetLength(FPpf,FListSize+1);
         SetLength(Fpresentkvar,FListSize+1);
         SetLength(FpresentkW,FListSize+1);
         SetLength(FAvgpVuPrior, FListSize+1);
         SetLength(FPresentVpu, FListSize+1);

         SetLength(NPhasesPVSys,FListSize+1);
         SetLength(NCondsPVSys,FListSize+1);
         SetLength(CondOffset,FListSize+1);
         SetLength(cBuffer,FListSize+1,7);  // assuming no more than 6 conductors
         SetLength(FPendingChange,FListSize+1);
         SetLength(QDeliver,FListSize+1);
         SetLength(QNew,FListSize+1);
         SetLength(QOld,FListSize+1);
         SetLength(QHeadroom,FListSize+1);
         SetLength(Qoutputpu,FListSize+1);
         SetLength(Qdesiredpu,FListSize+1);
         SetLength(PNew,FListSize+1);
         SetLength(POld,FListSize+1);
         SetLength(FVpuSolution,FListSize+1,2+1);
         SetLength(FWithinTol, FListSize+1);
         SetLength(FVregs, FListSize+1);
    End;  {Else}

  //Initialize arrays
  For i := 1 to FlistSize Do begin
    PVSys := PVSysClass.Find(FPVSystemNameList.Strings[i-1]);
    For j := 1 to 6 Do cBuffer[i,j]          := cZERO;
    Set_NTerms(PVSys.NTerms);
    FkWLimit[i]                              := 0.0;
    FkVALimit[i]                             := 0.0;
    FkvarLimit[i]                            := 0.0;
    FVref[i]                                 := 0.0;
    FPpf[i]                                  := 0.0;
    Fpresentkvar[i]                          := 0.0;
    FpresentkW[i]                            := 0.0;
    CondOffset[i]                            := 0;
    NPhasesPVSys[i]                          := PVSys.NPhases;
    NCondsPVSys[i]                           := PVSys.NConds;
    FAvgpVuPrior[i]                          := 0.0;
    FPresentVpu[i]                           := 0.0;
    QDeliver[i]                              := 0.0;
    QNew[i]                                  := 0.0;
    QOld[i]                                  := -1.0;
    PNew[i]                                  := 0.0;
    POld[i]                                  := -1.0;
    QHeadroom[i]                             :=0.0;
    Qoutputpu[i]                             :=0.0;
    Qdesiredpu[i]                            :=0.0;
    FWithinTol[i]                            := False;
    for j := 1 to 2 do  FVpuSolution[i,j]    :=0.0;
    FVregs[i] := 0.0; // this should later initialize from the volt/var curve zero crossing
    FPendingChange[i]                        := NONE;
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
  localControlledElement : TDSSCktElement;
  PVSys                  : TPVSystemObj;
  dt, Verr: Double; // for DYNAMICVREG
begin
  for j := 1 to FPVSystemPointerList.ListSize do begin
    // only update solution idx one time through this routine
    localControlledElement := ControlledElement[j];
    PVSys := localControlledElement as TPVSystemObj;
    FWithinTol[j] := False;
    dt :=  ActiveCircuit.Solution.Dynavars.h;
    Verr := FPresentVpu[j] - FVregs[j];
    FVregs[j] := FVregs[j] + Verr * (1 - Exp (-dt / FVregTau));
    PVSys.Set_Variable(5,FVregs[j]);
    If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name+','+PVSys.Name+',',
      Format('  **VREG set new FVreg= %.5g Vpu=%.5g Verr=%.5g Dec=%.5g',
      [FVregs[j], FPresentVpu[j], Verr, (1 - Exp (-dt/FVregTau))]));
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
