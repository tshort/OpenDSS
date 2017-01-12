unit ImplSwtControls;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;



type
  TSwtControls = class(TAutoObject, ISwtControls)
  protected
    function Get_Action: ActionCodes; safecall;
    function Get_AllNames: OleVariant; safecall;
    function Get_Delay: Double; safecall;
    function Get_First: Integer; safecall;
    function Get_IsLocked: WordBool; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    function Get_SwitchedObj: WideString; safecall;
    function Get_SwitchedTerm: Integer; safecall;
    procedure Set_Action(Value: ActionCodes); safecall;
    procedure Set_Delay(Value: Double); safecall;
    procedure Set_IsLocked(Value: WordBool); safecall;
    procedure Set_Name(const Value: WideString); safecall;
    procedure Set_SwitchedObj(const Value: WideString); safecall;
    procedure Set_SwitchedTerm(Value: Integer); safecall;
    function Get_Count: Integer; safecall;
    function Get_NormalState: ActionCodes; safecall;
    procedure Set_NormalState(Value: ActionCodes); safecall;
    function Get_State: ActionCodes; safecall;
    procedure Set_State(Value: ActionCodes); safecall;
    procedure Reset; safecall;

  end;

implementation

uses ComServ, DSSGlobals, Executive, ControlElem, SwtControl, Variants, SysUtils, PointerList;

function ActiveSwtControl: TSwtControlObj;
begin
  Result := nil;
  if ActiveCircuit[ActiveActor] <> Nil then Result := ActiveCircuit[ActiveActor].SwtControls.Active;
end;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit[ActiveActor]) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('swtcontrol.%s.%s=%s', [ActiveSwtControl.Name, parm, val]);
  DSSExecutive.Command := cmd;
end;

function TSwtControls.Get_Action: ActionCodes;
var
  elem: TSwtControlObj;
begin
  Result := dssActionNone;
  elem := ActiveSwtControl;
  if elem <> nil then begin
    Case elem.CurrentAction of
      CTRL_OPEN: Result := dssActionOpen;
      CTRL_CLOSE: Result := dssActionClose;
    End;
  end;
end;

function TSwtControls.Get_AllNames: OleVariant;
Var
  elem: TSwtControlObj;
  lst: TPointerList;
  k: Integer;
Begin
  Result := VarArrayCreate([0, 0], varOleStr);
  Result[0] := 'NONE';
  IF ActiveCircuit[ActiveActor] <> Nil THEN WITH ActiveCircuit[ActiveActor] DO
  If SwtControls.ListSize > 0 Then
  Begin
    lst := SwtControls;
    Result := VarArrayCreate([0, lst.ListSize-1], varOleStr);
    k:=0;
    elem := lst.First;
    WHILE elem<>Nil DO Begin
      Result[k] := elem.Name;
      Inc(k);
      elem := lst.Next;
    End;
  End;
end;

function TSwtControls.Get_Delay: Double;
var
  elem: TSwtControlObj;
begin
  Result := 0.0;
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.TimeDelay;
end;

function TSwtControls.Get_First: Integer;
Var
  elem: TSwtControlObj;
  lst:  TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then begin
    lst := ActiveCircuit[ActiveActor].SwtControls;
    elem := lst.First;
    If elem <> Nil Then Begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := elem;
          Result := 1;
        End
        Else elem := lst.Next;
      Until (Result = 1) or (elem = nil);
    End;
  End;
end;

function TSwtControls.Get_IsLocked: WordBool;
var
  elem: TSwtControlObj;
begin
  Result := FALSE;
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.IsLocked;   // Fixed bug here
end;

function TSwtControls.Get_Name: WideString;
var
  elem: TSwtControlObj;
begin
  Result := '';
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.Name;
end;

function TSwtControls.Get_Next: Integer;
Var
  elem: TSwtControlObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then Begin
    lst := ActiveCircuit[ActiveActor].SwtControls;
    elem := lst.Next;
    if elem <> nil then begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := elem;
          Result := lst.ActiveIndex;
        End
        Else elem := lst.Next;
      Until (Result > 0) or (elem = nil);
    End
  End;
end;

function TSwtControls.Get_SwitchedObj: WideString;
var
  elem: TSwtControlObj;
begin
  Result := '';
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.ElementName;
end;

function TSwtControls.Get_SwitchedTerm: Integer;
var
  elem: TSwtControlObj;
begin
  Result := 0;
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.ElementTerminal;
end;

procedure TSwtControls.Set_Action(Value: ActionCodes);
var
  elem: TSwtControlObj;
begin
  elem := ActiveSwtControl;
  if elem <> nil then begin
    Case Value of
      dssActionOpen:  elem.CurrentAction := CTRL_OPEN;
      dssActionClose: elem.CurrentAction := CTRL_CLOSE;
      dssActionReset: elem.Reset;
      dssActionLock:  elem.Locked := TRUE;
      dssActionUnlock: elem.Locked := FALSE;
      else // TapUp, TapDown, None have no effect
    End;
    // Make sure the NormalState has an initial value  before taking action
    if elem.NormalState = CTRL_NONE then
       case value of
          dssActionOpen:  elem.NormalState := CTRL_OPEN;
          dssActionClose: elem.NormalState := CTRL_CLOSE;
       end;
  end;
end;

procedure TSwtControls.Set_Delay(Value: Double);
var
  elem: TSwtControlObj;
begin
  elem := ActiveSwtControl;
  if elem <> nil then begin
      elem.TimeDelay  := Value;
  end;
end;

procedure TSwtControls.Set_IsLocked(Value: WordBool);
var
  elem: TSwtControlObj;
begin
  elem := ActiveSwtControl;
  if elem <> nil then begin
      elem.Locked := Value;
  end;

end;

procedure TSwtControls.Set_Name(const Value: WideString);
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TSwtControlObj;
  lst: TPointerList;
Begin
  IF ActiveCircuit[ActiveActor] <> NIL THEN Begin
    lst := ActiveCircuit[ActiveActor].SwtControls;
    S := Value;  // Convert to Pascal String
    Found := FALSE;
    ActiveSave := lst.ActiveIndex;
    elem := lst.First;
    While elem <> NIL Do Begin
      IF (CompareText(elem.Name, S) = 0) THEN Begin
        ActiveCircuit[ActiveActor].ActiveCktElement := elem;
        Found := TRUE;
        Break;
      End;
      elem := lst.Next;
    End;
    IF NOT Found THEN Begin
      DoSimpleMsg('SwtControl "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);    // Restore active Load
      ActiveCircuit[ActiveActor].ActiveCktElement := elem;
    End;
  End;
end;

procedure TSwtControls.Set_SwitchedObj(const Value: WideString);
begin
  Set_Parameter ('SwitchedObj', Value);
end;

procedure TSwtControls.Set_SwitchedTerm(Value: Integer);
begin
  Set_Parameter ('SwitchedTerm', IntToStr (Value));
end;

function TSwtControls.Get_Count: Integer;
begin
     If Assigned(ActiveCircuit[ActiveActor]) Then
             Result := ActiveCircuit[ActiveActor].SwtControls.ListSize;
end;

function TSwtControls.Get_NormalState: ActionCodes;
Var
  elem: TSwtControlObj;
begin
  elem := ActiveSwtControl;
  if elem <> nil then begin
      case elem.NormalState  of
        CTRL_OPEN: Result := dssActionOpen;
      else
        Result := dssActionClose;
      end;
  end;

end;

procedure TSwtControls.Set_NormalState(Value: ActionCodes);
Var
  elem: TSwtControlObj;
begin
  elem := ActiveSwtControl;
  if elem <> nil then begin
     case Value of
         dssActionOpen:  elem.NormalState := CTRL_OPEN;
     else
         elem.NormalState := CTRL_CLOSE;
     end;
  end;
end;

function TSwtControls.Get_State: ActionCodes;
var
  elem: TSwtControlObj;
begin
  Result := dssActionNone;
  elem   := ActiveSwtControl;
  if elem <> nil then begin
    Case elem.PresentState   of
      CTRL_OPEN:  Result := dssActionOpen;
      CTRL_CLOSE: Result := dssActionClose;
    End;
  end;
end;

procedure TSwtControls.Set_State(Value: ActionCodes);
var
  elem: TSwtControlObj;
begin
  elem   := ActiveSwtControl;
  if elem <> nil then begin
    Case value   of
      dssActionOpen:  elem.PresentState := CTRL_OPEN;
      dssActionClose: elem.PresentState := CTRL_CLOSE;
    End;
  end;

end;

procedure TSwtControls.Reset;
var
  elem: TSwtControlObj;
begin
  elem   := ActiveSwtControl;
  if elem <> nil then begin
      elem.Locked := FALSE;
      elem.Reset;
  end;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TSwtControls, Class_SwtControls,
    ciInternal, tmApartment);
end.
