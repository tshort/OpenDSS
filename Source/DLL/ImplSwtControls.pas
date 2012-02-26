unit ImplSwtControls;

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

  end;

implementation

uses ComServ, DSSGlobals, Executive, ControlElem, SwtControl, Variants, SysUtils, PointerList;

function ActiveSwtControl: TSwtControlObj;
begin
  Result := nil;
  if ActiveCircuit <> Nil then Result := ActiveCircuit.SwtControls.Active;
end;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
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
      CLOSE: Result := dssActionClose;
      OPEN: Result := dssActionOpen;
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
  IF ActiveCircuit <> Nil THEN WITH ActiveCircuit DO
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
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then begin
    lst := ActiveCircuit.SwtControls;
    elem := lst.First;
    If elem <> Nil Then Begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit.ActiveCktElement := elem;
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
  if elem <> nil then Result := elem.IsIsolated;
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
  If ActiveCircuit <> Nil Then Begin
    lst := ActiveCircuit.SwtControls;
    elem := lst.Next;
    if elem <> nil then begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit.ActiveCktElement := elem;
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
      dssActionOpen: Set_Parameter('Action', 'o');
      dssActionClose: Set_Parameter('Action', 'c');
      dssActionReset: begin  // Reset means the shelf state
        Set_Parameter('Lock', 'n');
        Set_Parameter('Action', 'c');
      end;
      dssActionLock: Set_Parameter('Lock', 'y');
      dssActionUnlock: Set_Parameter('Lock', 'n');
      else // TapUp, TapDown, None have no effect
    End;
  end;
end;

procedure TSwtControls.Set_Delay(Value: Double);
begin
  Set_Parameter ('Delay', FloatToStr (Value));
end;

procedure TSwtControls.Set_IsLocked(Value: WordBool);
begin
  If Value = TRUE then
    Set_Parameter ('Lock', 'y')
  else
    Set_Parameter ('Lock', 'n');
end;

procedure TSwtControls.Set_Name(const Value: WideString);
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TSwtControlObj;
  lst: TPointerList;
Begin
  IF ActiveCircuit <> NIL THEN Begin
    lst := ActiveCircuit.SwtControls;
    S := Value;  // Convert to Pascal String
    Found := FALSE;
    ActiveSave := lst.ActiveIndex;
    elem := lst.First;
    While elem <> NIL Do Begin
      IF (CompareText(elem.Name, S) = 0) THEN Begin
        ActiveCircuit.ActiveCktElement := elem;
        Found := TRUE;
        Break;
      End;
      elem := lst.Next;
    End;
    IF NOT Found THEN Begin
      DoSimpleMsg('SwtControl "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);    // Restore active Load
      ActiveCircuit.ActiveCktElement := elem;
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
     If Assigned(ActiveCircuit) Then
             Result := ActiveCircuit.SwtControls.ListSize;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TSwtControls, Class_SwtControls,
    ciInternal, tmApartment);
end.
