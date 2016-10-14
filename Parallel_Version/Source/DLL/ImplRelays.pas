unit ImplRelays;
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
  TRelays = class(TAutoObject, IRelays)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get_First: Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_MonitoredObj: WideString; safecall;
    procedure Set_MonitoredObj(const Value: WideString); safecall;
    function Get_MonitoredTerm: Integer; safecall;
    function Get_SwitchedObj: WideString; safecall;
    procedure Set_MonitoredTerm(Value: Integer); safecall;
    procedure Set_SwitchedObj(const Value: WideString); safecall;
    function Get_SwitchedTerm: Integer; safecall;
    procedure Set_SwitchedTerm(Value: Integer); safecall;
    function Get_idx: Integer; safecall;
    procedure Set_idx(Value: Integer); safecall;

  end;

implementation

uses ComServ, Executive, Relay, Circuit, DSSGlobals, Sysutils, Pointerlist, Variants;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('Relay.%s.%s=%s', [TRelayObj(RelayClass.GetActiveObj).Name, parm, val]);
  DSSExecutive.Command := cmd;
end;

function TRelays.Get_AllNames: OleVariant;
Var
  elem: TRelayObj;
  pList: TPointerList;
  k: Integer;
Begin
  Result := VarArrayCreate([0, 0], varOleStr);
  Result[0] := 'NONE';
  IF ActiveCircuit <> Nil THEN
  Begin
      If RelayClass.ElementList.ListSize > 0 then
      Begin
        pList := RelayClass.ElementList;
        VarArrayRedim(Result, pList.ListSize -1);
        k:=0;
        elem := pList.First;
        WHILE elem<>Nil DO Begin
            Result[k] := elem.Name;
            Inc(k);
            elem := pList.next        ;
        End;
      End;
  End;

end;

function TRelays.Get_Count: Integer;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then
        Result := RelayClass.ElementList.ListSize;
end;

function TRelays.Get_First: Integer;
Var
   pElem : TRelayObj;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := RelayClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := RelayClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
end;


function TRelays.Get_Name: WideString;
Var
  elem: TRelayObj;
Begin
  Result := '';
  elem := RelayClass.GetActiveObj;
  If elem <> Nil Then Result := elem.Name;
end;

function TRelays.Get_Next: Integer;
Var
   pElem : TRelayObj;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := RelayClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := RelayClass.ElementList.ActiveIndex;
          End
          Else pElem := RelayClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
end;

procedure TRelays.Set_Name(const Value: WideString);
// Set element active by name

begin
     If ActiveCircuit <> Nil Then
     Begin
          If RelayClass.SetActive(Value) Then
          Begin
               ActiveCircuit.ActiveCktElement := RelayClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Relay "'+ Value +'" Not Found in Active Circuit.', 77003);
          End;
     End;
end;

function TRelays.Get_MonitoredObj: WideString;
var
  elem: TRelayObj;
begin
  Result := '';
  elem := RelayClass.GetActiveObj  ;
  if elem <> nil then Result := elem.MonitoredElementName;
end;

procedure TRelays.Set_MonitoredObj(const Value: WideString);
var
  elem: TRelayObj;
begin
  elem := RelayClass.GetActiveObj ;
  if elem <> nil then Set_parameter('monitoredObj', Value);
end;

function TRelays.Get_MonitoredTerm: Integer;
var
  elem: TRelayObj;
begin
  Result := 0;
  elem := RelayClass.GetActiveObj  ;
  if elem <> nil then Result := elem.MonitoredElementTerminal ;
end;

function TRelays.Get_SwitchedObj: WideString;
var
  elem: TRelayObj;
begin
  Result := '';
  elem := RelayClass.ElementList.Active ;
  if elem <> nil then Result := elem.ElementName  ;

end;



procedure TRelays.Set_MonitoredTerm(Value: Integer);
var
  elem: TRelayObj;
begin
  elem := RelayClass.GetActiveObj ;
  if elem <> nil then Set_parameter('monitoredterm', IntToStr(Value));

end;

procedure TRelays.Set_SwitchedObj(const Value: WideString);
var
  elem: TRelayObj;
begin
  elem := RelayClass.GetActiveObj ;
  if elem <> nil then Set_parameter('SwitchedObj', Value);

end;

function TRelays.Get_SwitchedTerm: Integer;
var
  elem: TRelayObj;
begin
  Result := 0;
  elem := RelayClass.ElementList.Active   ;
  if elem <> nil then Result := elem.ElementTerminal  ;
end;

procedure TRelays.Set_SwitchedTerm(Value: Integer);
var
  elem: TRelayObj;
begin
  elem := RelayClass.GetActiveObj ;
  if elem <> nil then Set_parameter('SwitchedTerm', IntToStr(Value));
end;

function TRelays.Get_idx: Integer;
begin
    if ActiveCircuit <> Nil then
       Result := RelayClass.ElementList.ActiveIndex
    else Result := 0;
end;

procedure TRelays.Set_idx(Value: Integer);
Var
    pRelay:TRelayObj;
begin
    if ActiveCircuit <> Nil then   Begin
        pRelay := Relayclass.Elementlist.Get(Value);
        If pRelay <> Nil Then ActiveCircuit.ActiveCktElement := pRelay;
    End;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TRelays, Class_Relays,
    ciInternal, tmApartment);
end.
