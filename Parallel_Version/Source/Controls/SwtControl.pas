unit SwtControl;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2016, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------}

interface
USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex;

TYPE

  TSwtControl = class(TControlClass)
    protected
      PROCEDURE DefineProperties;
      FUNCTION MakeLike(const SwtControlName:String):Integer; override;
    public
      constructor Create;
      destructor Destroy; override;

      FUNCTION Edit(ActorID : Integer):Integer; override;     // uses global parser
      FUNCTION NewObject(const ObjName:String):Integer; override;
  end;

  TSwtControlObj = class(TControlElem)
    private
      FPresentState  : EControlAction;
      FNormalState   : EControlAction;
      ActionCommand  : EControlAction;
      LockCommand    : EControlAction;
      FLocked        : Boolean;
      Armed          : Boolean;

      PROCEDURE InterpretSwitchAction(const Action:String);
    procedure Set_NormalState(const Value: EControlAction);
    procedure set_Flocked(const Value: Boolean);
    procedure Set_LastAction(const Value: EControlAction);
    procedure Set_PresentState(const Value: EControlAction);
    public
      constructor Create(ParClass:TDSSClass; const SwtControlName:String);
      destructor Destroy; override;

      PROCEDURE MakePosSequence(ActorID : Integer); Override;  // Make a positive Sequence Model
      PROCEDURE RecalcElementData(ActorID : Integer); Override;
      PROCEDURE CalcYPrim(ActorID : Integer); Override;    // Always Zero for a SwtControl

      PROCEDURE Sample(ActorID : Integer);  Override;    // Sample control quantities and set action times in Control Queue
      PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer); Override;   // Do the action that is pending from last sample
      PROCEDURE Reset; Override;  // Reset to initial defined state

      PROCEDURE GetCurrents(Curr: pComplexArray; ActorID : Integer); Override; // Get present value of terminal Curr
      PROCEDURE GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override;   // Returns Injextion currents

      FUNCTION  GetPropertyValue(Index:Integer):String;Override;
      PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
      PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;
      Property NormalState: EControlAction   Read FNormalState Write Set_NormalState;
      Property PresentState: EControlAction  Read FPresentState Write Set_PresentState;
      Property IsLocked: Boolean Read FLocked;
      Property Locked: Boolean   Read Flocked write set_Flocked;
      Property CurrentAction: EControlAction Read ActionCommand Write Set_LastAction;
   end;

VAR
    ActiveSwtControlObj:TSwtControlObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit, Sysutils, Utilities, solution;

CONST

    NumPropsThisClass = 8;

constructor TSwtControl.Create;  // Creates superstructure for all SwtControl objects
Begin
     Inherited Create;

     Class_name   := 'SwtControl';
     DSSClassType := DSSClassType + SWT_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

destructor TSwtControl.Destroy;

Begin
     Inherited Destroy;
End;

PROCEDURE TSwtControl.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count

     AllocatePropertyArrays;   {see DSSClass}

     PropertyName[1] := 'SwitchedObj';
     PropertyName[2] := 'SwitchedTerm';
     PropertyName[3] := 'Action';
     PropertyName[4] := 'Lock';
     PropertyName[5] := 'Delay';
     PropertyName[6] := 'Normal';
     PropertyName[7] := 'State';
     PropertyName[8] := 'Reset';

     PropertyHelp[1] := 'Name of circuit element switch that the SwtControl operates. '+
                        'Specify the full object class and name.';
     PropertyHelp[2] := 'Terminal number of the controlled element switch. ' +
                        '1 or 2, typically.  Default is 1.';
     PropertyHelp[3] := '{Open | Close}  After specified delay time, and if not locked, causes the controlled switch to open or close. ' ;
     PropertyHelp[4] := '{Yes | No} Delayed action. Sends CTRL_LOCK or CTRL_UNLOCK message to control queue. ' +
                        'After delay time, controlled switch is locked in its present open / close state or unlocked. ' +
                        'Switch will not respond to either manual (Action) or automatic (COM interface) control or internal OpenDSS Reset when locked.';
     PropertyHelp[5] := 'Operating time delay (sec) of the switch. Defaults to 120.';
     PropertyHelp[6] := '{Open | Closed] Normal state of the switch. If not Locked, the switch reverts to this state for reset, change of mode, etc.' +
                        ' Defaults to first Action or State specified if not specifically declared.';
     PropertyHelp[7] := '{Open | Closed] Present state of the switch. Upon setting, immediately forces state of switch.';
     PropertyHelp[8] := '{Yes | No} If Yes, forces Reset of switch to Normal state and removes Lock independently of any internal '+
                        'reset command for mode change, etc.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

FUNCTION TSwtControl.NewObject(const ObjName:String):Integer;
Begin
    // Make a new SwtControl and add it to SwtControl class list
    WITH ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TSwtControlObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;

FUNCTION TSwtControl.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

Begin

  // continue parsing WITH contents of Parser
  ActiveSwtControlObj := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveSwtControlObj;

  Result := 0;

  WITH ActiveSwtControlObj Do Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0
         THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties)
         THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
           {internal SwtControl Property commands}
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 382);
            1: ElementName     := lowercase(Param);
            2: ElementTerminal := Parser.IntValue;
            3: InterpretSwitchAction (param);
            4: Locked := InterpretYesNo (Param);
            5: TimeDelay := Parser.DblValue;
            6: Begin    // set the normal state
                 InterpretSwitchAction (param);
                 NormalState := ActionCommand;
               End;
            7: Begin    // set the present state
                 InterpretSwitchAction (param);
                 PresentState := ActionCommand;
               End;
            8: If InterpretYesNo (Param) Then Begin  // force a reset
                  Locked := FALSE;
                  Reset;
                  PropertyValue[8]  := 'n';
               End;

         ELSE
           // Inherited parameters
           ClassEdit( ActiveSwtControlObj, ParamPointer - NumPropsthisClass)
         End;

         {supplemental actions}
         case ParamPointer of

             // Default to first action specified for legacy scripts
             3: if NormalState=CTRL_NONE then  NormalState := ActionCommand;

             4: if Locked then LockCommand :=  CTRL_LOCK else LockCommand := CTRL_UNLOCK;

             7: Begin
                  if NormalState=CTRL_NONE then  NormalState := PresentState;
                  Case PresentState of     // Force state
                    CTRL_OPEN:  ControlledElement.Closed[0] := FALSE;
                    CTRL_CLOSE: ControlledElement.Closed[0] := TRUE;
                  End;
                End;
         end;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     RecalcElementData(ActorID);
  End;

End;

FUNCTION TSwtControl.MakeLike(const SwtControlName:String):Integer;
VAR
   OtherSwtControl:TSwtControlObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this SwtControl name in the present collection}
   OtherSwtControl := Find(SwtControlName);
   IF OtherSwtControl<>Nil THEN
   WITH ActiveSwtControlObj Do
     Begin

        NPhases := OtherSwtControl.Fnphases;
        NConds  := OtherSwtControl.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherSwtControl.ElementName;
        ElementTerminal   := OtherSwtControl.ElementTerminal;
        ControlledElement := OtherSwtControl.ControlledElement;  // Pointer to target circuit element

        TimeDelay     := OtherSwtControl.TimeDelay;
        Locked        := OtherSwtControl.Locked;
        PresentState  := OtherSwtControl.PresentState;
        NormalState   := OtherSwtControl.NormalState;
        ActionCommand    := OtherSwtControl.ActionCommand;
        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherSwtControl.PropertyValue[i];

     End
   ELSE  DoSimpleMsg('Error in SwtControl MakeLike: "' + SwtControlName + '" Not Found.', 383);

End;

{==========================================================================}
{                    TSwtControlObj                                           }
{==========================================================================}

constructor TSwtControlObj.Create(ParClass:TDSSClass; const SwtControlName:String);
Begin
  Inherited Create(ParClass);
  Name := LowerCase(SwtControlName);
  DSSObjType := ParClass.DSSClassType;

  NPhases := 3;  // Directly set conds and phases
  Fnconds := 3;
  Nterms := 1;  // this forces allocation of terminals and conductors in base class

  ElementName   := '';
  ControlledElement := NIL;
  ElementTerminal := 1;
  PresentState  := CTRL_CLOSE;  // default to closed
  NormalState   := CTRL_NONE;   // default to unspecified; set on first setting action or anything
  ActionCommand := PresentState;
  Lockcommand   := CTRL_NONE;
  Locked        := FALSE;
  Armed         := FALSE;
  TimeDelay     := 120.0; // 2 minutes

  InitPropertyValues(0);
End;

destructor TSwtControlObj.Destroy;
Begin
  Inherited Destroy;
End;

PROCEDURE TSwtControlObj.RecalcElementData(ActorID : Integer);
VAR
  DevIndex :Integer;
Begin
  Devindex := GetCktElementIndex(ElementName);
  IF DevIndex>0 THEN Begin
    ControlledElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
    Nphases := ControlledElement.NPhases;
    Nconds  := FNphases;
    ControlledElement.ActiveTerminalIdx := ElementTerminal;

    ControlledElement.HasSwtControl := TRUE;  // For Reliability calcs
{
    if not Locked then
      Case PresentState of
        CTRL_OPEN: ControlledElement.Closed[0] := FALSE;
        CTRL_CLOSE: ControlledElement.Closed[0] := TRUE;
      End;

}
    // attach controller bus to the switch bus - no space allocated for monitored variables
    Setbus (1, ControlledElement.GetBus(ElementTerminal));
  End ELSE Begin
    ControlledElement := nil;   // element not found
    DoErrorMsg('SwtControl: "' + Self.Name + '"', 'CktElement Element "'+ ElementName + '" Not Found.',
             ' Element must be defined previously.', 387);
  End;
End;

procedure TSwtControlObj.MakePosSequence(ActorID : Integer);
begin
  if ControlledElement <> Nil then begin
    Nphases := ControlledElement.NPhases;
    Nconds := FNphases;
    Setbus(1, ControlledElement.GetBus(ElementTerminal));
  end;
  inherited;
end;

{--------------------------------------------------------------------------}
PROCEDURE TSwtControlObj.CalcYPrim(ActorID : Integer);
Begin
  // leave YPrims as nil
End;

PROCEDURE TSwtControlObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);
VAR
   i:Integer;
Begin
  For i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

PROCEDURE TSwtControlObj.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);
Var i:Integer;
Begin
  FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

PROCEDURE TSwtControlObj.DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer);
Var ctrl_code : EControlAction;
begin
     ctrl_code := EControlAction(Code);  // change type
     ControlledElement.ActiveTerminalIdx := ElementTerminal;
     case Ctrl_Code of
          CTRL_LOCK:    Locked := TRUE;
          CTRL_UNLOCK:  Locked := FALSE;
     else
          if not Locked then begin
              if (Code = Integer(CTRL_OPEN)) and (PresentState = CTRL_CLOSE) then begin
                ControlledElement.Closed[0] := FALSE; // Open all phases of active terminal
                PresentState := CTRL_OPEN;
                AppendtoEventLog('SwtControl.'+Self.Name, 'Opened');
              end;
              if (Code = Integer(CTRL_CLOSE)) and (PresentState = CTRL_OPEN) then begin
                ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
                PresentState := CTRL_CLOSE;
                AppendtoEventLog('SwtControl.'+Self.Name, 'Closed');
              end;
              Armed := FALSE;  // reset the switch
          end;
     end;
end;

PROCEDURE TSwtControlObj.InterpretSwitchAction(const Action:String);
Begin
  If Not Locked Then begin
    Case LowerCase(Action)[1] of
      'o': ActionCommand := CTRL_OPEN;
    else    // default is closed
      ActionCommand := CTRL_CLOSE;
    End;

    {   Changed to delayed action
    if ControlledElement <> nil then begin
      ControlledElement.ActiveTerminalIdx := ElementTerminal;
      Case PresentState of
        CTRL_OPEN: ControlledElement.Closed[0] := FALSE;
        CTRL_CLOSE: ControlledElement.Closed[0] := TRUE;
      End;
    End;
    }

  end;
End;

PROCEDURE TSwtControlObj.Sample(ActorID : Integer);
begin

// push on the Lock command if any at the present time delay
  if LockCommand <> CTRL_NONE then
  With ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution Do begin
       ControlQueue.Push(DynaVars.intHour, Dynavars.t + TimeDelay, LockCommand, 0, Self);
       LockCommand := CTRL_NONE;  // reset the lock command for next time
  end;

  if (ActionCommand <> PresentState) and not Armed then   // we need to operate this switch
  With ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution Do begin
       ControlQueue.Push(DynaVars.intHour, Dynavars.t + TimeDelay, ActionCommand, 0, Self);
       Armed := TRUE;
  end;
  {ControlledElement.ActiveTerminalIdx := ElementTerminal;
  IF  ControlledElement.Closed [0]      // Check state of phases of active terminal
  THEN PresentState := CTRL_CLOSE
  ELSE PresentState := CTRL_OPEN; }
end;

procedure TSwtControlObj.set_Flocked(const Value: Boolean);
begin
     Flocked := Value;
end;

procedure TSwtControlObj.Set_LastAction(const Value: EControlAction);
begin
     ActionCommand := Value;

end;

procedure TSwtControlObj.Set_NormalState(const Value: EControlAction);
begin
  FNormalState := Value;
end;

procedure TSwtControlObj.Set_PresentState(const Value: EControlAction);
begin
  FPresentState := Value;
end;

PROCEDURE TSwtControlObj.DumpProperties(Var F:TextFile; Complete:Boolean);
VAR
  i:Integer;
Begin
  Inherited DumpProperties(F,Complete);
  WITH ParentClass Do
    For i := 1 to NumProperties Do Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[PropertyIdxMap[i]]);
  If Complete THEN Writeln(F);
End;

FUNCTION TSwtControlObj.GetPropertyValue(Index: Integer): String;
begin
  Result := '';
  case Index of
      1: Result := ElementName;
      2: Result := Format('%d',[ElementTerminal]);
      3: Case ActionCommand of
          CTRL_OPEN: Result := 'open';
         else
          {CTRL_CLOSE:} Result := 'close';
         End;
      4: If Locked then Result := 'Yes' else Result := 'No';
      5: Result := Format('%-.7g',[TimeDelay]);
      6: Case FNormalState of
          CTRL_OPEN: Result := 'open';
         else
          {CTRL_CLOSE:} Result := 'closed';
         End;
      7: Begin
             ControlledElement.ActiveTerminalIdx := ElementTerminal;
             If ControlledElement.Closed[0] then Result := 'Closed'
             else Result := 'open';
         End;
      8: Result := 'n';  // Always no; yes is executed immediately
  else
      Result := Inherited GetPropertyValue(Index);
  end;

end;

Procedure TSwtControlObj.Reset;
Begin
    if not Locked then   Begin
      PresentState   := NormalState;
      ActionCommand  := PresentState;
      Armed          := FALSE;
      IF ControlledElement <> NIL  THEN  Begin
          ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
          case FNormalState of
            CTRL_OPEN:  ControlledElement.Closed[0] := FALSE;
          else
            {CTRL_CLOSE:} ControlledElement.Closed[0] := TRUE;  // Close all phases of active terminal
          end;
      End;
    End;
end;

procedure TSwtControlObj.InitPropertyValues(ArrayOffset: Integer);
begin
  PropertyValue[1]  := ''; //'element';
  PropertyValue[2]  := '1'; //'terminal';
  PropertyValue[3]  := 'c';
  PropertyValue[4]  := 'n';
  PropertyValue[5]  := '120.0';
  PropertyValue[6]  := 'c';
  PropertyValue[7]  := 'c';
  PropertyValue[8]  := 'n';
  inherited  InitPropertyValues(NumPropsThisClass);
end;

end.
