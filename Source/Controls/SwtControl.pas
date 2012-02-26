unit SwtControl;

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

      FUNCTION Edit:Integer; override;     // uses global parser
      FUNCTION NewObject(const ObjName:String):Integer; override;
  end;

  TSwtControlObj = class(TControlElem)
    private
      PresentState   :EControlAction;
      Locked : Boolean;

      PROCEDURE InterpretSwitchAction(const Action:String);
    public
      constructor Create(ParClass:TDSSClass; const SwtControlName:String);
      destructor Destroy; override;

      PROCEDURE MakePosSequence; Override;  // Make a positive Sequence Model
      PROCEDURE RecalcElementData; Override;
      PROCEDURE CalcYPrim; Override;    // Always Zero for a SwtControl

      PROCEDURE Sample;  Override;    // Sample control quantities and set action times in Control Queue
      PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer); Override;   // Do the action that is pending from last sample
      PROCEDURE Reset; Override;  // Reset to initial defined state

      PROCEDURE GetCurrents(Curr: pComplexArray); Override; // Get present value of terminal Curr
      PROCEDURE GetInjCurrents(Curr: pComplexArray); Override;   // Returns Injextion currents

      FUNCTION  GetPropertyValue(Index:Integer):String;Override;
      PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
      PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;
      Property IsLocked: Boolean Read Locked;
      Property CurrentAction: EControlAction Read PresentState;
   end;

VAR
    ActiveSwtControlObj:TSwtControlObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit, Sysutils, Utilities;

CONST

    NumPropsThisClass = 5;

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

     PropertyHelp[1] := 'Name of circuit element switch that the SwtControl operates. '+
                  'Specify the full object class and name.';
     PropertyHelp[2] := 'Terminal number of the controlled element switch. ' +
                  '1 or 2, typically.  Default is 1.';
     PropertyHelp[3] := '{Open | Close}  simulates manual operation of the controlled switch to open or close, after a time delay. ' +
                  'Note: automatic operation requires use of the COM interface with an external control algorithm.';
     PropertyHelp[4] := 'Controlled switch is locked in its present open / close state. ' +
                  'Switch will not respond to either manual (Action) or automatic (COM interface) control until this Lock is removed.';
     PropertyHelp[5] := 'Operating time delay (sec) of the switch. Defaults to 120.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

FUNCTION TSwtControl.NewObject(const ObjName:String):Integer;
Begin
    // Make a new SwtControl and add it to SwtControl class list
    WITH ActiveCircuit Do
    Begin
      ActiveCktElement := TSwtControlObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

FUNCTION TSwtControl.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

Begin

  // continue parsing WITH contents of Parser
  ActiveSwtControlObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveSwtControlObj;

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
         ELSE
           // Inherited parameters
           ClassEdit( ActiveSwtControlObj, ParamPointer - NumPropsthisClass)
         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     RecalcElementData;
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
        Locked         := OtherSwtControl.Locked;
        PresentState   := OtherSwtControl.PresentState;

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
  PresentState  := CLOSE;
  Locked        := FALSE;
  TimeDelay := 120.0;

  InitPropertyValues(0);
End;

destructor TSwtControlObj.Destroy;
Begin
  Inherited Destroy;
End;

PROCEDURE TSwtControlObj.RecalcElementData;
VAR
  DevIndex :Integer;
Begin
  Devindex := GetCktElementIndex(ElementName);
  IF DevIndex>0 THEN Begin
    ControlledElement := ActiveCircuit.CktElements.Get(DevIndex);
    Nphases := ControlledElement.NPhases;
    Nconds  := FNphases;
    ControlledElement.ActiveTerminalIdx := ElementTerminal;
    if not Locked then
      Case PresentState of
        OPEN: ControlledElement.Closed[0] := FALSE;
        CLOSE: ControlledElement.Closed[0] := TRUE;
      End;
    // attach controller bus to the switch bus - no space allocated for monitored variables
    Setbus (1, ControlledElement.GetBus(ElementTerminal));
  End ELSE Begin
    ControlledElement := nil;   // element not found
    DoErrorMsg('SwtControl: "' + Self.Name + '"', 'CktElement Element "'+ ElementName + '" Not Found.',
             ' Element must be defined previously.', 387);
  End;
End;

procedure TSwtControlObj.MakePosSequence;
begin
  if ControlledElement <> Nil then begin
    Nphases := ControlledElement.NPhases;
    Nconds := FNphases;
    Setbus(1, ControlledElement.GetBus(ElementTerminal));
  end;
  inherited;
end;

{--------------------------------------------------------------------------}
PROCEDURE TSwtControlObj.CalcYPrim;
Begin
  // leave YPrims as nil
End;

PROCEDURE TSwtControlObj.GetCurrents(Curr: pComplexArray);
VAR
   i:Integer;
Begin
  For i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

PROCEDURE TSwtControlObj.GetInjCurrents(Curr: pComplexArray);
Var i:Integer;
Begin
  FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

PROCEDURE TSwtControlObj.DoPendingAction(Const Code, ProxyHdl:Integer);
begin
  if not Locked then begin
    ControlledElement.ActiveTerminalIdx := ElementTerminal;
    if (Code = Integer(OPEN)) and (PresentState = CLOSE) then begin
      ControlledElement.Closed[0] := FALSE; // Open all phases of active terminal
      AppendtoEventLog('SwtControl.'+Self.Name, 'Opened');
    end;
    if (Code = Integer(CLOSE)) and (PresentState = Open) then begin
      ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
      AppendtoEventLog('SwtControl.'+Self.Name, 'Closed');
    end;
  end;
end;

PROCEDURE TSwtControlObj.InterpretSwitchAction(const Action:String);
Begin
  If Not Locked Then begin
    Case LowerCase(Action)[1] of
      'o': PresentState := OPEN;
      'c': PresentState := CLOSE;
    End;
    if ControlledElement <> nil then begin
      ControlledElement.ActiveTerminalIdx := ElementTerminal;
      Case PresentState of
        OPEN: ControlledElement.Closed[0] := FALSE;
        CLOSE: ControlledElement.Closed[0] := TRUE;
      End;
    End;
  end;
End;

PROCEDURE TSwtControlObj.Sample;
begin
  ControlledElement.ActiveTerminalIdx := ElementTerminal;
  IF  ControlledElement.Closed [0]      // Check state of phases of active terminal
  THEN PresentState := CLOSE
  ELSE PresentState := OPEN;
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
  Result := Inherited GetPropertyValue(Index);
end;

// TODO: should Reset close the switch?
Procedure TSwtControlObj.Reset;
Begin
  PresentState   := CLOSE;
  Locked         := FALSE;
  IF ControlledElement <> NIL  THEN  Begin
    ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
    ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
  End;
end;

procedure TSwtControlObj.InitPropertyValues(ArrayOffset: Integer);
begin
  PropertyValue[1]  := ''; //'element';
  PropertyValue[2]  := '1'; //'terminal';
  PropertyValue[3]  := 'c';
  PropertyValue[4]  := 'n';
  PropertyValue[5]  := '120.0';
  inherited  InitPropertyValues(NumPropsThisClass);
end;

end.
